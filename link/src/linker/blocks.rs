use super::*;
use crate::memory::*;
use crate::segment::CodeSymbol;
use std::collections::{HashMap, HashSet};
use std::io;
use std::sync::Arc;

pub enum DataBlock {
    RW(WritableDataBlock),
    RO(ReadonlyDataBlock),
}

pub enum CodeBlock {
    RW(WritableCodeBlock),
    RX(ExecutableCodeBlock),
}

pub type PatchSymbolPointers = im::HashMap<String, RelocationPointer>;
pub type LinkedSymbolPointers = im::HashMap<String, RelocationPointer>;

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
    pub fn patch(
        mut self,
        pointers: PatchSymbolPointers,
        got: TableVersion,
        plt: TableVersion,
    ) -> LinkedBlock {
        match self {
            Self::Code(block) => patch_code(block, pointers, got, plt),
            Self::Data(block) => patch_data(block, pointers, got, plt),
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
    pub(crate) symbols: HashMap<String, *const ()>,
    pub(crate) internal: HashMap<String, *const ()>,
    pub(crate) relocations: Vec<CodeRelocation>,
}

#[derive(Debug)]
pub struct PatchCodeBlock {
    pub(crate) name: String,
    pub(crate) block: WritableCodeBlock,
    pub(crate) symbols: HashMap<String, *const ()>,
    pub(crate) internal: HashMap<String, *const ()>,
    pub(crate) externs: HashSet<String>,
    pub(crate) relocations: Vec<CodeRelocation>,
}

#[derive(Debug)]
pub struct WritableDataBlock(pub(crate) Block);
impl WritableDataBlock {
    pub fn new(block: Block) -> Self {
        Self(block)
    }

    pub fn size(&self) -> usize {
        self.0.size
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.0.as_mut_ptr()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_mut_slice(&self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn make_readonly_block(mut self) -> io::Result<ReadonlyDataBlock> {
        Ok(ReadonlyDataBlock(self.0.make_readonly_block()?))
    }
}

#[derive(Debug)]
pub struct WritableCodeBlock(pub(crate) Block);
impl WritableCodeBlock {
    pub fn new(block: Block) -> Self {
        Self(block)
    }

    pub fn size(&self) -> usize {
        self.0.size
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.0.as_mut_ptr()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_mut_slice(&self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn make_exec_block(mut self) -> io::Result<ExecutableCodeBlock> {
        Ok(ExecutableCodeBlock(self.0.make_exec_block()?))
    }
}

#[derive(Debug)]
pub struct ReadonlyDataBlock(pub(crate) Block);
impl ReadonlyDataBlock {
    pub fn size(&self) -> usize {
        self.0.size
    }
    pub fn as_ptr(&self) -> *const u8 {
        self.0.as_ptr() as *const u8
    }
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

#[derive(Debug)]
pub struct ExecutableCodeBlock(pub Block);
impl ExecutableCodeBlock {
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0.as_ptr()
    }
}
