// ============================================================
//  C++ reparse + worklist spike — the showpiece, in one file.
//  Branch: spike/cpp-support.  Design: docs/prompt-cpp-reparse.md
//
//  Open this in your editor and read top-to-bottom. The comments mark
//  exactly what the spike does at each site. To see the machinery run,
//  the matching tests are:
//    cargo test cpp_type_inference_through_macro_reparse -- --nocapture
//    cargo test cpp_reparse_obstacle_delta_report      -- --nocapture
// ============================================================

// ---- (1) the macro that breaks everything --------------------------
// An export/attribute macro — the single most common real-world idiom
// (every Windows / __declspec / visibility-controlled header).
#define API_EXPORT __attribute__((visibility("default")))

// ---- (2) the casualty ----------------------------------------------
// WITHOUT expansion, tree-sitter-cpp cannot place `API_EXPORT` between
// `class` and the name, so the WHOLE class reparses as a
// function_definition: the `Box` class symbol EVAPORATES. goto-def on
// `Box`, its `width` field, its methods — all dead.
//
// WITH the reparse seam (preprocess_validated): `API_EXPORT` expands to
// its body `__attribute__((visibility("default")))`, which tree-sitter
// parses fine in that position — the macro was only HIDING valid
// syntax. The class comes back, error-free, validated by the parser
// itself (ERROR-count must not rise, or the expansion is discarded).
class API_EXPORT Box {
public:
    int width;        // <- recovered as a real field after reparse
    Box* clone() const;  // <- pointer-returning method (a Tier-1 gap
                         //    this spike found + fixed in skeleton.scm)
};

int main() {
    // ---- (3) the worklist, layered --------------------------------
    // C++'s type leak is DECLARED types — pervasive. Every declaration
    // below pushes a witness into the SAME production bag the Perl
    // engine uses, and the SAME reducer registry answers the queries.
    // Zero engine edits; it's a language the engine never heard of.

    int n = 5;             // -> Numeric   (primitive annot + literal agree)
    std::string s = "hi";  // -> String
    Box b;                 // -> ClassName("Box")  — RESOLVABLE only
                           //    because reparse brought the class back

    auto m = n;            // -> Numeric   via the 3-hop edge chase:
                           //    m -> Expr(n) -> Variable(n) -> Numeric
    auto same = b;         // -> ClassName("Box")  — edge chase on a
                           //    macro-recovered class

    // Temporality is free: above THIS line, `m` has no type yet — the
    // bag never guesses the future (try a hover before vs after).
    return 0;
}
