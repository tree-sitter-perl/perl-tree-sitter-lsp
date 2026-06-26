//! SPIKE: A2 — `#ifdef` conditional selection by config (Class A of
//! `~/personal/resume/research-static-analysis.md`).
//!
//! A2 is the doc's "pure pain": a `#if` that splits ONE construct (return
//! type from function name) shatters the parse into ERROR nodes, and —
//! unlike macro expansion — you cannot expand your way out. You must
//! CHOOSE a configuration: is `DEBUG` defined? That choice is the
//! `-D`/include-path coupling that makes C tooling build-coupled. The
//! honest input is a USER-SUPPLIED config (or `compile_commands.json`);
//! conditions the lite evaluator can't decide are handed to a real-`cpp`
//! PROBE under that config (amortized once — see the macro story).
//!
//! The transform: evaluate each conditional against the config, then
//! BLANK the dead branches and every directive line IN PLACE — replace
//! their bytes with spaces, keep newlines. Offsets are preserved, so the
//! re-parse's spans equal the original's: no anchor map. The probe
//! confirmed `int #if 0 foo #else main #endif (void){}` (4 ERROR nodes)
//! blanks to a clean `function_definition` named `main` at its original
//! byte position.
//!
//! Not wired into the pipeline; measured by `c_preproc_tests.rs`.

use std::collections::HashMap;

/// The configuration: which macros are defined, and (optionally) to what.
/// A bare `-D X` has value `"1"`, matching cpp.
#[derive(Debug, Default, Clone)]
pub struct Config {
    defined: HashMap<String, String>,
}

impl Config {
    pub fn new() -> Self {
        Config::default()
    }
    /// `("DEBUG", "1")`, `("VERSION", "3")`, ...
    pub fn with(mut self, name: &str, value: &str) -> Self {
        self.defined.insert(name.to_string(), value.to_string());
        self
    }
    pub fn define(&mut self, name: &str, value: &str) {
        self.defined.insert(name.to_string(), value.to_string());
    }
    pub fn is_defined(&self, name: &str) -> bool {
        self.defined.contains_key(name)
    }
}

/// A condition the lite evaluator could not decide — the handoff to a
/// real-`cpp` probe under the same config.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unresolved {
    pub line: usize,
    pub condition: String,
}

#[derive(Debug)]
pub struct Selection {
    /// Source with dead branches + directives blanked in place; offsets
    /// preserved, so re-parse spans equal the original.
    pub source: String,
    /// Conditions the lite evaluator punted — feed these to the probe.
    pub unresolved: Vec<Unresolved>,
}

#[derive(Clone, Copy)]
enum Cond {
    True,
    False,
    Unknown,
}

/// One open conditional level.
struct Frame {
    /// Was the enclosing context live? (false → whole block is dead).
    parent_live: bool,
    /// Has any arm been taken yet (gates `#elif`/`#else`).
    taken: bool,
    /// Is the CURRENT arm live (parent_live && this arm selected).
    live: bool,
}

/// Select the live configuration of `src`, blanking dead branches and
/// directive lines in place. Unresolvable conditions are recorded and
/// (conservatively) treated as false.
pub fn select_config(src: &str, config: &Config) -> Selection {
    let mut out = String::with_capacity(src.len());
    let mut unresolved = Vec::new();
    let mut stack: Vec<Frame> = Vec::new();
    let cur_live = |st: &[Frame]| st.last().map_or(true, |f| f.live);

    for (lineno, raw) in src.split_inclusive('\n').enumerate() {
        let line = raw.strip_suffix('\n').unwrap_or(raw);
        let trimmed = line.trim_start();
        let directive = trimmed
            .strip_prefix('#')
            .map(|r| r.trim_start())
            .and_then(|r| {
                let kw: String = r.chars().take_while(|c| c.is_alphabetic()).collect();
                let rest = r[kw.len()..].trim();
                (!kw.is_empty()).then(|| (kw, rest.to_string()))
            });

        match directive.as_ref().map(|(k, r)| (k.as_str(), r)) {
            Some(("if", rest)) | Some(("ifdef", rest)) | Some(("ifndef", rest)) => {
                let parent_live = cur_live(&stack);
                let cond = if !parent_live {
                    Cond::False
                } else {
                    let kw = &directive.as_ref().unwrap().0;
                    let c = eval(kw, rest, config);
                    if matches!(c, Cond::Unknown) {
                        unresolved.push(Unresolved { line: lineno, condition: format!("#{} {}", kw, rest) });
                    }
                    c
                };
                let live = parent_live && matches!(cond, Cond::True);
                stack.push(Frame { parent_live, taken: live, live });
                blank_line(&mut out, line);
            }
            Some(("elif", rest)) => {
                if let Some(f) = stack.last_mut() {
                    if f.parent_live && !f.taken {
                        let c = eval("if", rest, config);
                        if matches!(c, Cond::Unknown) {
                            unresolved.push(Unresolved { line: lineno, condition: format!("#elif {rest}") });
                        }
                        f.live = matches!(c, Cond::True);
                        f.taken |= f.live;
                    } else {
                        f.live = false;
                    }
                }
                blank_line(&mut out, line);
            }
            Some(("else", _)) => {
                if let Some(f) = stack.last_mut() {
                    f.live = f.parent_live && !f.taken;
                    f.taken = true;
                }
                blank_line(&mut out, line);
            }
            Some(("endif", _)) => {
                stack.pop();
                blank_line(&mut out, line);
            }
            // non-conditional directives (#define/#include/#pragma...) are
            // not our concern here — blank them so they can't corrupt the
            // parse, but only when live (dead ones blank anyway).
            Some(_) => blank_line(&mut out, line),
            None => {
                if cur_live(&stack) {
                    out.push_str(line);
                } else {
                    blank_line(&mut out, line);
                }
            }
        }
        if raw.ends_with('\n') {
            out.push('\n');
        }
    }
    Selection { source: out, unresolved }
}

/// Replace every byte of a line with a space (offsets preserved).
fn blank_line(out: &mut String, line: &str) {
    out.extend(std::iter::repeat(' ').take(line.len()));
}

/// The lite condition evaluator. Handles the shapes that dominate real
/// embedded code; everything else is `Unknown` → the probe's job.
fn eval(kw: &str, rest: &str, config: &Config) -> Cond {
    let b = |x: bool| if x { Cond::True } else { Cond::False };
    match kw {
        "ifdef" => b(config.is_defined(rest)),
        "ifndef" => b(!config.is_defined(rest)),
        "if" => eval_if_expr(rest, config),
        _ => Cond::Unknown,
    }
}

/// `#if` expressions: integer literals, `defined(X)` / `defined X`, and a
/// single leading `!`. Anything compound (`&&`, `||`, arithmetic, macro
/// values) is `Unknown` — deliberately, so it routes to the probe rather
/// than being guessed.
fn eval_if_expr(expr: &str, config: &Config) -> Cond {
    let e = expr.trim();
    if let Some(inner) = e.strip_prefix('!') {
        return match eval_if_expr(inner, config) {
            Cond::True => Cond::False,
            Cond::False => Cond::True,
            Cond::Unknown => Cond::Unknown,
        };
    }
    if let Ok(n) = e.parse::<i64>() {
        return if n != 0 { Cond::True } else { Cond::False };
    }
    if let Some(name) = e
        .strip_prefix("defined")
        .map(|r| r.trim().trim_start_matches('(').trim_end_matches(')').trim())
    {
        if name.chars().all(|c| c.is_alphanumeric() || c == '_') && !name.is_empty() {
            return if config.is_defined(name) { Cond::True } else { Cond::False };
        }
    }
    Cond::Unknown
}

#[cfg(test)]
#[path = "c_preproc_tests.rs"]
mod tests;
