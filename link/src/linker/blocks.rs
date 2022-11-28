use super::*;
use crate::memory::*;
use std::collections::{HashMap, HashSet};
use std::io;
use std::sync::Arc;
use std::error::Error;

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
    Code(PatchBlock),
    DataRO(PatchBlock),
    DataRW(PatchBlock),
}


#[derive(Debug)]
pub enum PatchBlockKind {
    Code,
    Data,
}


/*
#[derive(Debug)]
pub enum PatchBlock {
    Code(PatchCodeBlock),
    Data(PatchDataBlock),
}
impl PatchBlock {
    pub fn disassemble(&self) {
        match self {
            Self::Code(block) => block.disassemble(),
            Self::Data(block) => block.disassemble(),
        }
    }
}
*/

#[derive(Debug)]
pub struct PatchBlock {
    pub(crate) kind: PatchBlockKind,
    pub(crate) name: String,
    pub(crate) block: Block,
    pub(crate) externs: HashSet<String>,
    pub(crate) symbols: HashMap<String, RelocationPointer>,
    pub(crate) internal: HashMap<String, RelocationPointer>,
    pub(crate) relocations: Vec<CodeRelocation>,
}
impl PatchBlock {
    pub fn patch(
        mut self,
        pointers: PatchSymbolPointers,
        got: TableVersion,
        plt: TableVersion,
    ) -> Result<LinkedBlock, Box<dyn Error>> {
        let block = match self.kind {
            PatchBlockKind::Code => patch_code(self, pointers, got, plt),
            PatchBlockKind::Data => patch_data(self, pointers, got, plt),
        };
        block.finalize()
    }

    pub fn finalize(mut self) -> Result<LinkedBlock, Box<dyn Error>> {
        match self.kind {
            PatchBlockKind::Code => {
                Ok(LinkedBlock(Arc::new(LinkedBlockInner::Code(self.make_executable()?))))
            }
            PatchBlockKind::Data => {
                Ok(LinkedBlock(Arc::new(LinkedBlockInner::DataRW(self))))
            }
        }
    }

    pub fn make_readonly(mut self) -> io::Result<Self> {
        self.block = self.block.make_readonly_block()?;
        Ok(self)
    }

    pub fn make_executable(mut self) -> io::Result<Self> {
        self.block = self.block.make_exec_block()?;
        Ok(self)
    }
}

#[derive(Debug)]
pub struct PatchDataBlock {
    pub(crate) name: String,
    pub(crate) block: WritableDataBlock,
    pub(crate) symbols: HashMap<String, RelocationPointer>,
    pub(crate) internal: HashMap<String, RelocationPointer>,
    pub(crate) relocations: Vec<CodeRelocation>,
}

#[derive(Debug)]
pub struct PatchCodeBlock {
    pub(crate) name: String,
    pub(crate) block: WritableCodeBlock,
    pub(crate) symbols: HashMap<String, RelocationPointer>,
    pub(crate) internal: HashMap<String, RelocationPointer>,
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
