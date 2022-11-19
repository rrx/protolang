use super::*;
use crate::memory::*;
use std::sync::Arc;

pub type PatchSymbolPointers = im::HashMap<String, *const ()>;
pub type LinkedSymbolPointers = im::HashMap<String, *const ()>;

#[derive(Clone, Debug)]
pub struct LinkedBlock(pub Arc<LinkedBlockInner>);
impl LinkedBlock {
    pub fn disassemble(&self) {
        //let pointers = im::HashMap::new();
        let inner = &self.0.as_ref();
        match inner {
            LinkedBlockInner::Code(block) => block.disassemble(), //disassemble_code(block.as_slice(), pointers),
            LinkedBlockInner::DataRO(block) => block.disassemble(), //disassemble_data(block.as_slice(), pointers),
            LinkedBlockInner::DataRW(block) => block.disassemble(), //disassemble_data(block.as_slice(), pointers),
        }
    }
}
#[derive(Debug)]
pub enum LinkedBlockInner {
    Code(ExecutableCodeBlock),
    DataRO(ReadonlyDataBlock),
    DataRW(WritableDataBlock),
}

#[derive(Debug)]
pub enum PatchBlock {
    Code(PatchCodeBlock),
    Data(PatchDataBlock),
}
impl PatchBlock {
    pub fn patch(mut self, pointers: PatchSymbolPointers) -> (LinkedBlock, LinkedSymbolPointers) {
        match self {
            Self::Code(block) => patch_code(block, &pointers),
            Self::Data(block) => patch_data(block, &pointers),
        }
    }
    pub fn disassemble(&self) {
        match self {
            Self::Code(block) => block.disassemble(),
            Self::Data(block) => block.disassemble(),
        }
    }
}

#[derive(Debug)]
pub struct PatchDataBlock {
    pub(crate) name: String,
    pub(crate) block: WritableDataBlock,
    pub(crate) symbols: im::HashMap<String, *const ()>,
    pub(crate) relocations: im::HashMap<String, CodeRelocation>,
}

#[derive(Debug)]
pub struct PatchCodeBlock {
    pub(crate) name: String,
    pub(crate) block: WritableCodeBlock,
    pub(crate) symbols: im::HashMap<String, *const ()>,
    pub(crate) unknowns: im::HashSet<String>,
    pub(crate) relocations: im::HashMap<String, CodeRelocation>,
}
