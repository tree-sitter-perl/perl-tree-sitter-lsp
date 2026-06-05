# linked-editing

Verified `linked-editing` (linkedEditingRange) rows. Positions are 0-based input / 1-based output. Each row reproduces against the current usability-sprint binary.

| id | difficulty | semantic_area | repo | cursor | expected | actual | verdict | confidence |
|----|------------|---------------|------|--------|----------|--------|---------|------------|
| le-uri-scheme-local | simple | classic | URI | `lib/URI.pm:151:8` `$scheme` | 12 ranges: lexical $scheme local to sub implementor (decl L152 + 11 uses), scoped to sub body only | 12 ranges: 152:8, 153:10, 153:21, 158:5, 158:18, 162:31, 163:38, 164:21, 168:26, 173:17, 195:28, 196:17 | PASS | gold |
| le-uri-implements-hash | simple | classic | URI | `lib/URI.pm:12:3` `%implements` | 5 ranges: file-level my %implements (decl L13 + $implements{} at L162,164,168,196); POD prose (L435) excluded | 5 ranges: 13:4, 162:19, 164:9, 168:14, 196:5 | PASS | gold |
| le-uri-scheme-re-our | tricky | classic | URI | `lib/URI.pm:23:5` `$scheme_re` | 14 ranges: package our $scheme_re (decl L24) + regex-interpolated uses incl. FQ $URI::scheme_re at L126 | 14 ranges: 24:5, 66:21, 73:35, 101:44, 126:29, 153:35, 225:35, 232:52, 233:29, 240:33, 242:26, 245:33, 269:17, 273:18 | PASS | gold |
| le-uri-implementor-sub-def | tricky | classic | URI | `lib/URI.pm:149:4` `implementor` | arguably 2 ranges: sub def (L150) + bareword call implementor($scheme) at L77 | no linked ranges (need ≥2 occurrences) | REVIEW | provisional |
| le-moo-target-closure | tricky | moo | Moo | `lib/Moo.pm:94:11` `$target` | 10 ranges: $target in _gen_subs captured across nested anonymous sub closures | 10 ranges: 95:12, 98:30, 99:37, 104:41, 105:37, 119:37, 121:34, 122:33, 123:39, 131:29 | PASS | gold |
| le-moo-me-closure | simple | moo | Moo | `lib/Moo.pm:94:7` `$me` | 7 ranges: $me in _gen_subs (decl L95 + uses L98,99,105,119,121,123); L104 uses Moo::Role not $me | 7 ranges: 95:7, 98:7, 99:7, 105:7, 119:9, 121:9, 123:9 | PASS | gold |
| le-moo-class-is-class | simple | moo | Moo | `lib/Moo.pm:81:11` `$class` | 3 ranges: $class in is_class (decl L82 + two $MAKERS{$class} on L83); hash key 'is_class' NOT matched | 3 ranges: 82:12, 83:18, 83:37 | PASS | gold |
| le-moo-me-single-occurrence | simple | moo | Moo | `lib/Moo.pm:81:7` `$me` | no linked ranges (single occurrence) — $me in is_class declared L82 but never used | no linked ranges (need ≥2 occurrences) | PASS | gold |
| le-moo-makers-our-fq-and-strings | tricky | moo | Moo | `lib/Moo.pm:34:4` `%MAKERS` | 13 ranges: our %MAKERS (decl L35) + $MAKERS{}/$Moo::MAKERS{} uses; FQ inside single-quoted eval strings (L211,212) NOT matched | 13 ranges: 35:5, 57:16, 83:10, 83:29, 154:24, 177:17, 178:3, 185:11, 185:35, 203:17, 204:3, 235:27, 268:16 | PASS | gold |
| le-dt-p-hash-slices | tricky | type-inference | DateTime | `lib/DateTime.pm:190:7` `%p` | 30 ranges: %p in _new (decl L191) + every $p{...}/@p{...} through L256 incl. hash slices and the 3 real $p{time_zone} refs in the paren-wrapped ternary at L211,212,213 | 27 ranges — MISSING the three $p{time_zone} refs at L211/L212/L213 | FAIL | gold |
| le-dt-class-new | simple | classic | DateTime | `lib/DateTime.pm:189:7` `$class` | 6 ranges: $class in _new (decl L190 + uses L194,204,206,216,219) | 6 ranges: 190:8, 194:16, 204:22, 206:26, 216:30, 219:11 | PASS | gold |
| le-dt-infinity-constant | tricky | constants | DateTime | `lib/DateTime.pm:81:4` `INFINITY` | arguably 4+ ranges: constant sub INFINITY def (L82) + bareword uses (L84 twice, 1823, 1981) | no linked ranges (need ≥2 occurrences) | REVIEW | provisional |

## Known-failing detail

- **le-dt-p-hash-slices** — three real-code `$p{time_zone}` references inside the parenthesized ternary RHS (`ref $p{time_zone} ? $p{time_zone} : ...new(name=>$p{time_zone})`) at L211/212/213 are dropped (27 of 30). String-interpolated `$p{second}` at L256 IS caught, so the gap is specific to the paren-wrapped ternary — a genuine builder ref-emission gap (rule #7). Counter-case `le-dt-class-new` confirms `$class` on the same construct's neighbouring lines is captured, so the drop is construct-specific.

## Provisional detail

- **le-uri-implementor-sub-def** / **le-dt-infinity-constant** — bareword sub def/call pairs (a plain `sub` and a `use constant` / `sub INFINITY () {...}` constant) do not participate in linkedEditing. A rename user would want the link, but linkedEditing intentionally only co-edits linked occurrences; the expected is soft ("arguably"). Both consistent with the same bareword-sub non-linking class.

## Rejected (excluded from corpus)

None.
