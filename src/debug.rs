fn backtrace_callers() -> Vec<String> {
    let mut backtrace = std::backtrace::Backtrace::force_capture()
        .to_string()
        .replace("\\", "/");
    backtrace = backtrace
        .lines()
        .filter(|l| l.contains("at ./") && l.contains("/src/"))
        .collect();
    let mut callers: Vec<String> = backtrace
        .trim()
        .replace("at ./", "")
        .split_whitespace()
        .filter_map(|s| {
            if s.is_empty() {
                None
            } else {
                Some(s.to_string())
            }
        })
        .rev()
        .collect();
    callers.pop();
    callers.dedup();
    if callers.is_empty() {
        callers = vec![String::new()];
    }
    callers
}

pub fn backtrace(skips: usize) -> String {
    let mut callers = backtrace_callers();
    callers.resize(callers.len().saturating_sub(skips + 1), String::new());
    callers.join(" > ")
}

/// for nicer error messages with pretty backtrace info
pub fn set_panic_hook() {
    std::panic::set_hook(Box::new(|panic_info| {
        let panic = |s: &str| {
            println!(
                "panicked: \x1b[38;2;241;76;76m{}\x1b[0m\n\x1b[2m{}\x1b[0m",
                s,
                backtrace(2)
            );
        };
        if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            panic(s);
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            panic(s);
        } else {
            panic("")
        }
    }));
}
