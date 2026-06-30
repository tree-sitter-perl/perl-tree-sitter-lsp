; Perl value-flow capture pack — the assignment SHAPES, declarative.
;
; Run inside build() by `mint_flow_edges_via_query` (NOT the spike extractor),
; so FlowEdges carry the BUILDER's scope. Capture vocabulary:
;
;   @flow.lhs      a `my`/`local`/`our` declaration LHS (single OR list) —
;                  the minter iterates its slots (positional for a list)
;   @flow.target   a bare scalar LHS (reassignment) — a single Whole target
;   @flow.source   the value expression the LHS receives
;
; The minter reuses `lhs_list_targets`/`list_element_nodes` for the positional
; pairing — the shape is declared here, the pairing logic is shared. STRUCTURAL
; forms where the `right:` field misses a parenthesized RHS (a tree-sitter-perl
; quirk; see the perl-query-field-quirk note).

; `my $x = EXPR` / `my @a = EXPR` / `my ($a, $b) = EXPR` — bare RHS.
(assignment_expression
  left: (variable_declaration) @flow.lhs
  right: (_) @flow.source)

; parenthesized-list RHS (`= (1, 2)`, `= @arr` is bare so above): the `right:`
; field points at `(`, so match the list_expression structurally.
(assignment_expression
  left: (variable_declaration) @flow.lhs
  (list_expression) @flow.source)

; bare reassignment: `$x = EXPR`
(assignment_expression
  left: (scalar) @flow.target
  right: (_) @flow.source)
