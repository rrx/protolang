use crate::memory::*;
use std::sync::Arc;

#[derive(Clone)]
pub struct SmartBlock {
    heap: HeapBlock,
}

impl SmartBlock {
    pub fn new(heap: HeapBlock) -> Self {
        Self { heap }
    }
}

#[derive(Clone)]
pub struct SyncPointer(Arc<SmartPointer>);
impl SyncPointer {
    pub fn new(p: SmartPointer) -> Self {
        Self(Arc::new(p))
    }
}

#[derive(Clone)]
pub struct TableVersion {
    block: SmartBlock,
    entries: im::HashMap<String, SyncPointer>,
}

impl TableVersion {
    pub fn new(block: SmartBlock) -> Self {
        Self {
            block,
            entries: im::HashMap::new(),
        }
    }

    pub fn create_buffer(&mut self, size: usize) -> SmartPointer {
        self.block.heap.alloc(size).unwrap()
    }

    // update and return a new version of the table
    pub fn update(mut self, name: String, p: SmartPointer) -> Self {
        self.entries.insert(name, SyncPointer::new(p));
        self
    }

    pub fn debug(&self) {
        eprintln!("Table@{:#08x}", self.block.heap.base() as usize);
        for (k, v) in &self.entries {
            unsafe {
                let ptr = v.0.as_ptr() as *const usize;
                eprintln!(" {:#08x}:*{:#08x}:{}", ptr as usize, *ptr, k);
            }
        }
    }
}
