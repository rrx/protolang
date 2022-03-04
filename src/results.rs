#[derive(Debug, Clone)]
pub enum Results {
    Warning(String, usize),
    Error(String, usize),
}
