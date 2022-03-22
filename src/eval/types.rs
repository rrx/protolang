
#[derive(Clone)]
pub enum TypeModifier {
    Ref,
    Default
}

#[derive(Clone)]
pub struct TypeSig {
    modifier: TypeModifier,
    traits: Vec<Trait>
}

impl Default for TypeSig {
    fn default() -> Self {
        Self { modifier: TypeModifier::Default, traits: vec![] }
    }
}

#[derive(Clone)]
pub struct ErrorSig {}

#[derive(Clone)]
pub enum FunctionModifier {
    Pure,
    Default
}

#[derive(Clone)]
pub struct FunctionSig {
    modifier: FunctionModifier,
    args: Vec<TypeSig>,
    ret: TypeSig,
    err: Vec<ErrorSig>
}

impl FunctionSig {
    // take ownership, return ownership
    fn id() -> Self {
        let arg = TypeSig::default();
        let ret = TypeSig::default();
        Self { 
            modifier: FunctionModifier::Pure,
            args: vec![arg],
            ret,
            err: vec![]
        }
    }

    fn single_pure(arg: TypeSig) -> Self {
        let ret = arg.clone();
        Self { 
            modifier: FunctionModifier::Pure,
            args: vec![arg],
            ret,
            err: vec![]
        }
    }
}


#[derive(Clone)]
pub struct Trait {
    sigs: Vec<FunctionSig>
}


