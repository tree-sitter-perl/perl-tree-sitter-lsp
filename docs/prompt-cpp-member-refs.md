# Refactor: cpp member/macro access → mint refs core already resolves

**Why.** cpp member resolution was built as a *parallel stack* — a cursor-time
resolver bolted onto every consumer (`backend::pack_member_at`,
`pack_xfile_word_at`, the goto-def/hover fallback chains) plus four ancestor
walks (`member_def_site`, `member_hover`, `field_type_on_class`,
`complete_members_for_class`). Core already resolves Perl `$obj->method`
through ONE shape: a `RefKind::MethodCall { invocant, invocant_span,
method_name_span }`, typed query-time by `method_call_invocant_class` →
`expr_type_at_span(invocant_span)`, dispatched by `resolve_method_in_ancestors`
→ `method_resolution_on_class`. `find_definition` / `refs_to` / rename / hover
all flow from that one ref. cpp never emitted it, so it never reused it.

**The fix: make cpp mint the same ref.** Then everything flows through core
and the parallel stack DELETES.

## The slices (each verified before the next)

### 1. Mint the ref
- `queries/cpp/skeleton.scm`: capture a member ACCESS (not only the existing
  method-CALL): `(field_expression argument: (_) @member.recv field:
  (field_identifier) @ref.member)`. `@member.recv` is the receiver subtree
  (its span = the invocant_span).
- `SkelRef` gains `invocant_span: Option<Span>` (None for plain calls).
- `into_file_analysis`: a `@ref.member` SkelRef → `RefKind::MethodCall {
  invocant: InvocantName::from(recv text), invocant_span: Some(recv span),
  method_name_span: field span }`. The existing cpp method-CALL capture should
  mint MethodCall too (today cpp only emits FunctionCall/Variable — that's why
  even C++ `obj.method()` never used the invocant machinery).

### 2. Receiver typing = the witness already there + ONE edge
- A variable receiver already emits `Expr(span)` (query_extract.rs:1040-1060),
  so `expr_type_at_span(invocant_span)` types it for free.
- Transparent wrappers `(*p)`, `(&o)`, `(p)` (the deref-paren the cursor path
  peels today): emit `Expr(wrapper_span) → Edge(Expr(inner_span))` at
  extraction. The peel becomes the canonical "edges, not values" edge; core's
  edge-chase resolves it. The wrapper node-kinds are PACK-declared (reuse
  `LangPack`, see slice 5) — core branches on no grammar name.

### 3. Teach resolution that a data Field counts
- `method_resolution_on_class` (file_analysis.rs:6944) matches only `Sub |
  Method`. Add `Variable | Field` so a cpp data member resolves to its def
  symbol. This is the ONLY core-resolution change.

### 4. Delete the parallel stack
- DELETE `backend::pack_member_at`, `pack_xfile_word_at`, and the member/word
  fallback arms of `goto_definition` + `hover` — `ref_at → find_definition`
  now handles them. DELETE `file_analysis::member_def_site`, `member_hover`.
- KEEP `field_type_on_class` (it serves COMPLETION chain typing in
  `cursor_sentinel::resolve_node_type`, a legitimately cursor-time path) — but
  FIX its dropped cross-file branch (the live bug: it scans only `self.symbols`
  while its siblings also check `get_cached(cls)`).
- KEEP `cursor_sentinel` ONLY for completion (incomplete parse, no ref yet).
- Macros (`OP_NULL`, `BASEOP`): a macro usage that survives as an identifier
  should likewise be a ref core resolves cross-file; the `pack_xfile_word_at`
  raw-word + `#define`-line re-grep goes away once def-ness is a modeled
  symbol property (don't re-grep source for `"#define"`).

### 5. Fold the parallel CONFIG seam (from the reviews)
- `LangCfg` (cursor_sentinel.rs) duplicates `LangPack` (`member_kinds` declared
  in both, already forked cpp vs python) and hardcodes `kind()=="call_expression"`
  in core shared code — so Python `obj.method().attr` silently won't type.
  Fold `LangCfg` into `LangPack` (`wrapper_kinds`, `skip_kinds`, `call_kinds`,
  explicit `operator_correctable`); reach it via the driver; delete `lang_cfg()`.

### 6. Cross-cutting cleanups the reviews flagged
- `Symbol::display_type(ty)` — one "name: class + deref stars" projection;
  hover/member adopt it AND inlay-hints/sig-help adopt it (today the latter two
  drop the pointer `*`s — a real inconsistency).
- `layering_tests.rs` grows Lsp-layer teeth: `backend.rs`/`symbols.rs` may name
  no `child_by_field_name`/`TreeCursor`/`descendant_for_*`/`std::fs::read*`
  (route through `cursor_sentinel`/`CrossFileLookup`). Without this the boundary
  keeps eroding per language.
- The ~24 `== "perl"` string branches → `LanguageDriver` capability methods
  (`cheap_synchronous_build()`, `has_preprocessor()`, `wants_enrichment()`).
- Delete dead `Document::rebuild_analysis` (unused; its doc lies about its
  return) — `spawn_debounced_rebuild` builds off-lock via `spawn_blocking` +
  `apply_rebuilt`.

## Status

**Landed (verified 1080/0, cpp gold 214/0, Perl e2e 112/0, cpp e2e 112/0):**
slices 1–4 (mint the MethodCall ref + `peel_recv` + `method_resolution_on_class`
`Variable|Field` + delete `pack_member_at` / `member_def_site` / backend member
fallbacks), hover via the ref, `Symbol::display_type` (hover + inlay — fixes the
vanishing stars), the four ancestor-walks collapsed onto
`resolve_method_in_ancestors`, the `field_type_on_class` cross-file bug, the
Python `call_kinds`/`simple_var_kinds` rule-#10 fix, dead `rebuild_analysis`.

**Residual — each a separate careful change, NOT a blocker on the above:**
1. **Full `LangCfg`→`LangPack` fold.** The correctness (Python call kind) is
   already fixed via `call_kinds`/`simple_var_kinds`. Merging `member_kinds`
   is blocked on generalizing the cpp-grammar-coupled `member_access_sites`
   op-DX walk to python's `attribute` node + an explicit `operator_correctable`
   flag — else a naive merge risks a python op-DX regression. The cpp
   `recv_wrapper_kinds` (LangPack) / `wrapper_kinds` (LangCfg) overlap dedups
   in the same move.
2. **Layering-test LSP-layer teeth.** Blocked on PRE-EXISTING Perl
   `descendant_for_point_range` in symbols.rs (1472/1545/1832) — strict teeth
   would force refactoring unrelated Perl first (else it's an allowlist).
3. **`==perl`→capability methods.** Per-branch design (the 22 branches span
   LSP handlers, CLI modes, caching — some fundamental, not capabilities); a
   blanket `is_pack()` is a half-measure.
4. **Macros (`OP_NULL`/`BASEOP`) as cross-file refs** → deletes
   `pack_xfile_word_at` + its `#define`-line re-grep (rule-#10) + the symbols
   cross-file `fs::read` (route through `CrossFileLookup`).

## Verification gates (every slice)
default `1080/0`, cpp gold `214/0`, Perl e2e `112/0`, cpp e2e green. End state:
goto-def / hover / references / rename on `o->op_type` AND `(*op_p)->op_type`
(real perl5 op.c) all resolve via `ref_at` with the backend member/word
fallbacks DELETED; `grep pack_member_at|pack_xfile_word_at|member_def_site|
member_hover src/` returns nothing.
