# workspace-symbol

Verified `workspace-symbol` rows (`--workspace-symbol`). Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary. Matching is case-insensitive substring (not subsequence — no gaps).

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| ws-moo-pkg | simple | oo-isa | Moo | `lib/Moo.pm:0:8` `Moo` | Package Moo @ Moo.pm:0 col 8 (kind Package) | col 8, kind Package, line 0, name Moo (among 533 matches) | PASS | gold |
| ws-uri-pkg | simple | oo-isa | URI | `lib/URI.pm:0:8` `URI` | Package URI @ URI.pm:0 col 8 (kind Package) | col 8, kind Package, line 0, name URI (222 total) | PASS | gold |
| ws-uri-newabs | simple | classic | URI | `lib/URI.pm:86:4` `new_abs` | Sub new_abs @ URI.pm:86 col 4 (kind Sub) | 3 matches: URI.pm:86:4, WithBase.pm:25:4, file.pm:46:4 (all Sub) | PASS | gold |
| ws-cat-pkg | simple | oo-isa | Catalyst-Runtime | `lib/Catalyst/Controller.pm:0:8` `Catalyst::Controller` | Package @ Controller.pm:0 col 8 (kind Package) | col 8, kind Package, line 0 (2 total) | PASS | gold |
| ws-cat-forward | tricky | classic | Catalyst-Runtime | `lib/Catalyst.pm:488:4` `forward` | Sub forward @ Catalyst.pm:488 col 4 (kind Sub) | col 4, kind Sub, line 488 (among 14 matches) | PASS | gold |
| ws-cat-toapp | tricky | codegen | Catalyst-Runtime | `lib/Catalyst.pm:3557:0` `to_app` | Sub to_app @ Catalyst.pm:3557 col 0 (kind Sub) | col 0, kind Sub, line 3557 (1 match; `*to_app = \&psgi_app;` typeglob alias) | PASS | gold |
| ws-dt-mon | tricky | codegen | DateTime | `lib/DateTime.pm:797:0` `mon` | Sub mon @ DateTime.pm:797 col 0 (kind Sub) | col 0, kind Sub, line 797 (among 46; `*mon = \&month;` glob alias) | PASS | gold |
| ws-dt-fromepoch | tricky | classic | DateTime | `lib/DateTime.pm:485:8` `from_epoch` | Sub from_epoch @ DateTime.pm:485 col 8 (kind Sub) | col 8, kind Sub, line 485 (1 match; 4-space indent reflected in col) | PASS | gold |
| ws-jsonpp-const | tricky | constants | JSON-PP | `lib/JSON/PP.pm:24:13` `P_ASCII` | Sub P_ASCII @ PP.pm:24 col 13 (kind Sub) | col 13, kind Sub, line 24 (use-constant codegen indexed as Sub) | PASS | gold |
| ws-moo-accessor | tricky | moo | Catalyst-Runtime | `lib/Catalyst/Action.pm:29:4` `namespace` | Method namespace @ Action.pm:29 col 4 (kind Method) + synth HashKeyDef same loc | Method namespace @ Action.pm:29:4 (x2) + HashKeyDef @ 29:4 (among 93) | PASS | gold |
| ws-moose-accessor | tricky | moose | Catalyst-Runtime | `lib/Catalyst/ActionContainer.pm:20:4` `part` | Method part @ ActionContainer.pm:20 col 4 (kind Method) + synth HashKeyDef same loc | Method part @ 20:4 (x2) + HashKeyDef @ same loc (among 41) | PASS | gold |
| ws-subexp-build | simple | exporter | Sub-Exporter | `lib/Sub/Exporter.pm:703:4` `build_exporter` | Sub build_exporter @ Exporter.pm:703 col 4 (kind Sub) | col 4, kind Sub, line 703 (1 match) | PASS | gold |
| ws-exptiny-import | tricky | exporter | Exporter-Tiny | `lib/Exporter/Tiny.pm:53:4` `import` | Sub import @ Tiny.pm:53 col 4 (kind Sub) | col 4, kind Sub, line 53 (among 19) | PASS | gold |
| ws-tt-coerce | tricky | type-inference | Type::Tiny | `lib/Type/Tiny.pm:1088:4` `coerce` | Sub coerce @ Tiny.pm:1088 col 4 (kind Sub) | col 4, kind Sub, line 1088 (among 39; repo dir name contains `::` and indexes fine) | PASS | gold |
| ws-cat-fuzzy | tricky | oo-isa | Catalyst-Runtime | `lib/Catalyst/Controller.pm:0:8` `Controll` (partial) | Package Catalyst::Controller via partial query 'Controll' | col 8, kind Package, line 0 (among 193 for 'Controll'). Substring semantics: 'newabs'->0, 'new_a'->3, 'NEW_ABS'->3. | PASS | gold |
