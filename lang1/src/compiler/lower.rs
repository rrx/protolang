pub trait CodeGenLower {
    type Input;
    type Output;
    type Error;
    fn lower(&mut self, i: &Self::Input) -> Result<Self::Output, Self::Error>;
}


