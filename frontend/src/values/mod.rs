use gazebo::prelude::*;
use std::ops::Add;
use std::ops::Sub;

pub mod string;

/// Index of a char in a string.
/// This is different from string byte offset.
#[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Clone, Dupe, Debug)]
pub(crate) struct CharIndex(pub(crate) usize);

impl Sub for CharIndex {
    type Output = CharIndex;

    fn sub(self, rhs: CharIndex) -> CharIndex {
        CharIndex(self.0 - rhs.0)
    }
}

impl Add for CharIndex {
    type Output = CharIndex;

    fn add(self, rhs: CharIndex) -> CharIndex {
        CharIndex(self.0 + rhs.0)
    }
}
