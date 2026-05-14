package App::PerlLSP::PluginGen::ImportBase;

# POC generator. Given an `Import::Base` subclass, read its
# `@IMPORT_MODULES` + `%IMPORT_BUNDLES` package variables and render a
# `.rhai` plugin that emits the right `SyntheticUse` actions per
# bundle flag.
#
# We `require` the kit (which populates `our`-vars at module compile
# time) but never invoke its `import` method — coderef bodies and
# bundle dispatch never run at generator time, so no probe package or
# `Import::Into` monkey-patching is needed.
#
# Coderef entries can't be statically decoded; the generator emits a
# `// TODO` comment with the kit-file location and skips them.
# Unimport entries (`-MOD` / `>-MOD`) get a TODO too — no
# `SyntheticUnuse` primitive exists yet on the LSP side.

use v5.42;
use utf8;
use warnings;
use experimental 'signatures';

use Carp qw(croak);
use B ();

our $VERSION = '0.001';

# --- Public entry --------------------------------------------------

# generate_plugin($kit_class, %opts) → string of .rhai source.
#
# Required:
#   $kit_class — package name of the Import::Base subclass.
#
# Optional:
#   lib_paths => [paths]    — prepended to @INC before `require`.
#   plugin_id => 'string'   — overrides the default `<kit>-generated` id.
#   aliases   => [classes]  — extra module names that should trigger
#                             this plugin (e.g. the install-wide shim
#                             `Co::Base` that forwards to
#                             `Co::Base::Local`).
sub generate_plugin ($kit_class, %opts) {
    my @libs = @{ $opts{lib_paths} // [] };
    local @INC = (@libs, @INC);

    eval "require $kit_class; 1"
        or croak "could not require $kit_class: $@";

    my ($modules_ref, $bundles_ref) = _read_tables($kit_class);

    my @base_entries = _walk_entries($modules_ref);
    my %bundle_entries =
        map { $_ => [ _walk_entries($bundles_ref->{$_}) ] }
        keys %$bundles_ref;

    return _render_rhai(
        kit_class => $kit_class,
        plugin_id => $opts{plugin_id} // _kit_to_plugin_id($kit_class),
        aliases   => $opts{aliases}   // [],
        base      => \@base_entries,
        bundles   => \%bundle_entries,
    );
}

# --- Reading the kit ----------------------------------------------

sub _read_tables ($kit_class) {
    no strict 'refs';
    my $modules_ref = *{"${kit_class}::IMPORT_MODULES"}{ARRAY} // [];
    my $bundles_ref = *{"${kit_class}::IMPORT_BUNDLES"}{HASH}  // {};
    return ( [@$modules_ref], { %$bundles_ref } );
}

# --- Walking + sigil classification --------------------------------

# Yield a normalized record per logical entry:
#   { kind => 'synthetic_use', module, args, imports, priority }
#   { kind => 'todo_unimport', module, args, via }
#   { kind => 'todo_coderef',  file, line }
#   { kind => 'todo_unknown',  raw }
#
# Import::Base stores entries in two shapes:
#   1. `MOD => [args]`  — pair: string key, arrayref value.
#   2. `MOD`            — bare scalar with no following arrayref.
# Coderefs appear as standalone scalars (no following arrayref).
sub _walk_entries ($list) {
    return () unless ref $list eq 'ARRAY';
    my @out;
    my $i = 0;
    while ( $i < @$list ) {
        my $entry = $list->[$i];

        if ( ref $entry eq 'CODE' ) {
            my ( $file, $line ) = _coderef_loc($entry);
            push @out, {
                kind => 'todo_coderef',
                file => $file // '?',
                line => $line // 0,
            };
            $i++;
            next;
        }

        # `MOD => [args]` — string key with following arrayref.
        if ( !ref $entry && $i + 1 < @$list && ref $list->[ $i + 1 ] eq 'ARRAY' ) {
            push @out, _classify_pair( $entry, $list->[ $i + 1 ] );
            $i += 2;
            next;
        }

        # Bare module name — no args.
        if ( !ref $entry ) {
            push @out, _classify_pair( $entry, [] );
            $i++;
            next;
        }

        # Anything else (hashref, blessed object, etc.) — flag.
        push @out, { kind => 'todo_unknown', raw => "$entry" };
        $i++;
    }
    return @out;
}

# Split a string-keyed entry into the canonical record. The prefix
# encodes Import::Base's "what kind of use line" sigil:
#
#   (none)   `use MOD args`               → synthetic_use, normal
#   >        `MOD->import::into($t,args)` → synthetic_use, normal
#                                           (static effect == use)
#   <        `use MOD 'X'` (base-class)   → synthetic_use, early
#                                           (emit before non-`<`)
#   -        `no MOD args`                → todo_unimport
#   >-       `MOD->unimport::out_of(...)` → todo_unimport
sub _classify_pair ( $name, $args ) {
    my ( $prefix, $module ) = $name =~ /\A( [><]? -? )(.+)\z/x;
    $prefix //= '';

    if ( $prefix eq '-' || $prefix eq '>-' ) {
        return {
            kind   => 'todo_unimport',
            module => $module,
            args   => [@$args],
            via    => $prefix eq '>-' ? 'import_into' : 'use',
        };
    }

    return {
        kind     => 'synthetic_use',
        module   => $module,
        args     => [@$args],
        imports  => [@$args],   # mirror real path's dual-fill
        priority => $prefix eq '<' ? 'early' : 'normal',
    };
}

sub _coderef_loc ($cv) {
    my $obj = B::svref_2object($cv);
    return ( undef, undef ) unless $obj;
    my $file = eval { $obj->FILE };
    my $line = eval { $obj->START && !$obj->START->isa('B::NULL')
        ? $obj->START->line
        : $obj->GV && !$obj->GV->isa('B::SPECIAL')
            ? $obj->GV->LINE
            : undef
    };
    return ( $file, $line );
}

# --- Rendering -----------------------------------------------------

sub _kit_to_plugin_id ($kit_class) {
    my $id = lc $kit_class;
    $id =~ s/::/-/g;
    return "${id}-generated";
}

sub _rhai_quote ($s) {
    my $copy = "$s";
    $copy =~ s/\\/\\\\/g;
    $copy =~ s/"/\\"/g;
    return qq("$copy");
}

sub _list_lit (@items) {
    return '[' . join( ', ', map { _rhai_quote($_) } @items ) . ']';
}

# Stable sort: `<`-prefixed entries first; rest in original order.
sub _stable_order (@entries) {
    my @early = grep { ( $_->{priority} // '' ) eq 'early' } @entries;
    my @rest  = grep { ( $_->{priority} // '' ) ne 'early' } @entries;
    return ( @early, @rest );
}

sub _render_entry ( $entry, $indent ) {
    my $kind = $entry->{kind};

    if ( $kind eq 'synthetic_use' ) {
        my $mod  = _rhai_quote( $entry->{module} );
        my $args = _list_lit( @{ $entry->{args} } );
        my $imps = _list_lit( @{ $entry->{imports} } );
        # Quote every map key. Rhai reserves a long list of words
        # (`module`, `package`, `class`, `import`, `export`, `super`,
        # `default`, `with`, ...) — quoting universally sidesteps the
        # game of "is this field name reserved?" as the generator grows
        # support for more EmitAction variants. The serde deserializer
        # on the LSP side accepts quoted keys transparently.
        return qq(${indent}out += #{ "SyntheticUse": #{ "module": $mod, "args": $args, "imports": $imps, "span": ctx.span }};);
    }
    if ( $kind eq 'todo_coderef' ) {
        return qq(${indent}// TODO: coderef at $entry->{file}:$entry->{line} — hand-author equivalent SyntheticUse / PackageParent emissions.);
    }
    if ( $kind eq 'todo_unimport' ) {
        my $args = join( ' ', @{ $entry->{args} } );
        return qq(${indent}// TODO: unimport $entry->{module} [$args] (via $entry->{via}) — no SyntheticUnuse primitive yet.);
    }
    if ( $kind eq 'todo_unknown' ) {
        return qq(${indent}// TODO: unknown entry shape: $entry->{raw});
    }
    return qq(${indent}// TODO: unhandled record kind: $kind);
}

sub _render_rhai (%args) {
    my $kit       = $args{kit_class};
    my $plugin_id = $args{plugin_id};
    my @aliases   = @{ $args{aliases} };
    my @base      = @{ $args{base} };
    my %bundles   = %{ $args{bundles} };

    my @trigger_modules = ( $kit, @aliases );
    my $gate = join ' &&' . "\n       ",
        map { qq(ctx.module_name != ) . _rhai_quote($_) } @trigger_modules;

    my @lines;
    push @lines, "// Generated by App::PerlLSP::PluginGen::ImportBase from $kit.";
    push @lines, "// DO NOT EDIT BY HAND — rerun the generator when the kit changes.";
    push @lines, "";
    push @lines, qq(fn id() { ) . _rhai_quote($plugin_id) . q( }) ;
    push @lines, q(fn triggers() { [ #{ Always: () } ] });
    push @lines, "";
    push @lines, q(fn on_use(ctx) {);
    push @lines, qq(    if $gate { return []; });
    push @lines, "";
    push @lines, q(    let out = [];);
    push @lines, "";
    push @lines, q(    // First non-dash arg in raw_args is the bundle name —);
    push @lines, q(    // matches Import::Base's `_parse_args` semantics: every);
    push @lines, q(    // leading non-`-` arg is a bundle, the loop stops at the);
    push @lines, q(    // first `-flag`. We accept the dash-prefixed form too);
    push @lines, q(    // (`-Plugin` as a synonym for `'Plugin'`) since some kits);
    push @lines, q(    // are documented that way even though Import::Base itself);
    push @lines, q(    // wouldn't recognize it as a bundle.);
    push @lines, q(    let bundle = "";);
    push @lines, q(    for arg in ctx.raw_args {);
    push @lines, q(        if arg == "" { continue; });
    push @lines, q(        bundle = if arg.starts_with("-") { arg.sub_string(1) } else { arg };);
    push @lines, q(        break;);
    push @lines, q(    });
    push @lines, "";

    push @lines, q(    // @IMPORT_MODULES — runs for every invocation.);
    for my $e ( _stable_order(@base) ) {
        push @lines, _render_entry( $e, "    " );
    }
    push @lines, "";

    for my $bundle ( sort keys %bundles ) {
        my @entries = @{ $bundles{$bundle} };
        next unless @entries;
        push @lines, qq(    if bundle == ) . _rhai_quote($bundle) . q( {);
        for my $e ( _stable_order(@entries) ) {
            push @lines, _render_entry( $e, "        " );
        }
        push @lines, q(    });
        push @lines, "";
    }

    push @lines, q(    out);
    push @lines, q(});
    push @lines, "";

    return join "\n", @lines;
}

1;

__END__

=head1 NAME

App::PerlLSP::PluginGen::ImportBase — generate `.rhai` plugins from
`Import::Base` subclasses for the perl-tree-sitter-lsp `SyntheticUse`
primitive.

=head1 SYNOPSIS

  use App::PerlLSP::PluginGen::ImportBase qw();

  my $rhai = App::PerlLSP::PluginGen::ImportBase::generate_plugin(
      'Co::Base::Local',
      lib_paths => ['lib', 'kit-common/lib'],
      aliases   => ['Co::Base'],
  );

  open my $fh, '>', 'plugins/co-base-local.rhai' or die $!;
  print $fh $rhai;

=head1 DESCRIPTION

See C<docs/prompt-importbase-plugin-gen.md> in the perl-tree-sitter-lsp
repo for the design rationale.
