# rename

Verified `rename` rows (`--rename`). Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| rename-01-mkopt-def | simple | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:429:4` `mkopt` | @EXPORT_OK :8, def :429, recursive :456 AND in-body calls :69 :104 :167 | ONLY :8, :429, :456. MISSED in-body calls :69 :104 :167. | FAIL | gold |
| rename-02-mkopt-callsite | tricky | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:69:12` `mkopt` | same complete set as row 1 | ONLY the 3 call sites :69 :104 :167. MISSED def :429, @EXPORT_OK :8, recursive :456. (Package resolves None.) | FAIL | gold |
| rename-03-mkopt-hash | simple | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:453:4` `mkopt_hash` | @EXPORT_OK :8 and def :453; no other uses | :8 and :453. Complete. | PASS | gold |
| rename-04-croak | tricky | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:19:4` `_croak` | def :19, @EXPORT_OK :8, in-body calls + \&_croak refs, cross-file FQ in Shiny.pm | :8,:19,:145,:164,:259,:298,:327,:360,:361,:362,:370,:402,:408 + Shiny.pm:24,31. Comment-only :341 skipped. | PASS | gold |
| rename-05-setup-exporter | tricky | exporter | Sub-Exporter | `lib/Sub/Exporter.pm:590:4` `setup_exporter` | def :590, call :931, qw() bareword :933, group-list bareword :937, cross-file FQ in tests | All caught: :590,:931,:933,:937 + GroupGen.pm:54, s_e.pm:8, faux-export.t:114, real-export-href.t:27/175, valid-config.t:33. | PASS | gold |
| rename-06-build-exporter | tricky | exporter | Sub-Exporter | `lib/Sub/Exporter.pm:703:4` `build_exporter` | def :703, in-body call :602, export bareword :933, call :934, cross-file FQ | def :703, :933, :934 + all cross-file FQ. MISSED in-body call :602. (Group list :937 reads `build_export` typo, correctly excluded.) | FAIL | gold |
| rename-07-path-segments | tricky | oo-isa | URI | `lib/URI/_generic.pm:121:4` `path_segments` | def :121 + cross-file callers inheriting _generic | def :121 + smb.pm:36. Win32/Unix/URL not touched. (See reject note — expected overclaimed.) | PASS | reject |
| rename-08-authority | tricky | oo-isa | URI | `lib/URI/_generic.pm:37:4` `authority` | def :37, cross-file in inheriting subclasses, intra-file dynamic locals (soft) | def :37 + _server.pm 7 sites + file.pm:38 + ldapi.pm:13/18. MISSED intra-file $abs/$rel/$base->canonical->authority (dynamic locals). | REVIEW | provisional |
| rename-09-base-constant | simple | constants | URI | `lib/URI/_punycode.pm:14:13` `BASE` | def :14 + all 11 uses | def :14 + 10 uses. MISSED use at :79 (`$w *= (BASE - $t)`). | FAIL | gold |
| rename-10-moo-has-accessor | tricky | moo | Moo | `t/accessor-coerce.t:38:6` `plus_three` | Foo's has decl :38 only; not other packages' has nor call sites (loop locals) | Method{name:plus_three, class:Foo}. ONLY :38. Others untouched. | REVIEW | provisional |
| rename-11-urn-canonical-precision | tricky | oo-isa | URI | `lib/URI/urn.pm:90:4` `canonical` | ONLY URI::urn::canonical :90; not 7 other same-named defs | ONLY urn.pm:90. All other canonical defs (incl. subclass urn/isbn override) untouched. | PASS | gold |

## Known-failing detail

- **rename-01 / rename-02 (mkopt)** — def-side and call-site rename give disjoint buckets. From-def catches @EXPORT_OK + def + same-scope recursive call but drops bareword calls in other sub bodies; from-callsite catches only the call-site bucket (package resolves None). Contrast `_croak` (rename-04), which catches its in-body calls — mkopt lacks a prototype, _croak has `($;@)`, parsed differently. Serious completeness defect.
- **rename-06-build-exporter** — same def-side in-body-call defect: `my $import = build_exporter($config)` inside setup_exporter (:602) dropped; top-level bareword/call and cross-file FQ all caught.
- **rename-09-base-constant** — use-constant rename nearly complete (10/11) but drops `BASE` in `$w *= (BASE - $t)` at :79; other `(BASE - $t)` occurrences caught — parse-specific miss.

## REVIEW-class detail

- **rename-08-authority** — cross-file inheritance precision strong; same-file dynamically-typed locals ($self->clone / URI->new) missed (defensible — not statically the def's class).
- **rename-10-moo-has-accessor** — correctly class-scoped to Foo; call sites are loop locals not statically typed to Foo (unresolvable). Precision correct, completeness not exercised.

## Rejected (excluded from corpus)

- **rename-07-path-segments** — REJECT (expected was incorrect). The collector marked Win32.pm/Unix.pm/URL.pm callers as renameable, but URL.pm uses `URI::WithBase` and Win32/Unix use `URI::file::Base`; neither has URI::_generic in its ISA, so those invocants are NOT URI::_generic objects. The tool correctly renames only the def and the one genuinely-inheriting caller (smb: smb->_login->_server->_generic). Not renaming the others is correct, not a defect; original FAIL verdict was wrong.
