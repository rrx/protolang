pub mod hir;

pub use hir::*;

#[macro_use]
pub mod util;

pub mod scan;
pub use scan::*;

pub mod testing;
pub mod visit;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
