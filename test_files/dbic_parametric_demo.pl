#!/usr/bin/env perl
# DBIC parametric ResultSet demo. Exercises the typing chain that
# residual Part 5c landed:
#
#   $schema->resultset('Schema::Result::User')
#       → Parametric(ResultSet { base: "DBIx::Class::ResultSet",
#                                row:  "Schema::Result::User" })
#   …->find(1)
#       → RowOf<that ResultSet> → ClassName("Schema::Result::User")
#   …->name
#       → resolves through the row's add_columns synthesis
#
# Plus the column-key narrowing on search:
#
#   $rs->search({ name => 'X' })
#       → `name` is a HashKeyAccess on Schema::Result::User
#
# Used by test_e2e_dbic_parametric.lua.

use strict;
use warnings;

# ── Inline schema. Real codebases split this into one file per
# package; the inline form keeps the demo self-contained and the
# e2e fixture single-file.

package Schema::Result::User;
use base 'DBIx::Class::Core';

__PACKAGE__->add_columns(
    id    => { data_type => 'integer', is_auto_increment => 1 },
    name  => { data_type => 'varchar', size => 64 },
    email => { data_type => 'varchar', size => 128 },
);

__PACKAGE__->set_primary_key('id');

package main;

# `$schema` is presumed; in real code it'd come from a connect() call.
my $schema;

# ── (a) chain: resultset -> find -> column accessor ──
#
# `$user` lands as ClassName("Schema::Result::User") via the
# RowOf<ResultSet { row }> projection. Hover on `$user` mentions
# User; completion on `$user->` narrows to its accessors.
my $user = $schema->resultset('Schema::Result::User')->find(1);
my $who  = $user->name;

# ── (b) column-key in search args ──
#
# Cursor on `name` in the hashref → HashKeyAccess on
# Schema::Result::User → goto-def lands on the add_columns line.
my $rs = $schema->resultset('Schema::Result::User');
my $rows = $rs->search({ email => 'x@y' });

1;
