use linked_list_allocator::*;
use memmap::*;
use std::alloc::Layout;
use std::error::Error;
use std::fmt;
use std::io;
use std::ptr::NonNull;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct BlockFactory(Arc<Mutex<BlockFactoryInner>>);

pub struct BlockFactoryInner {
    page_size: usize,
    m: MmapMut,
    code: Heap,
    data: Heap,
}

impl BlockFactory {
    pub fn create(
        num_code_pages: usize,
        num_data_pages: usize,
    ) -> Result<BlockFactory, Box<dyn Error>> {
        // the total amount of space allocated should not be more than 4GB,
        // because we are limited to 32bit relative addressing
        // we can address things outside this block, but we need 64 bit addressing
        let ps = page_size();
        let size_plus_metadata = ps * (num_code_pages + num_data_pages);
        let m = MmapMut::map_anon(size_plus_metadata)?;
        //unsafe {
        //libc::mprotect(m.as_ptr() as *mut libc::c_void, size_plus_metadata, 7);
        //}
        let mut data_heap = Heap::empty();
        let mut code_heap = Heap::empty();

        unsafe {
            let data_ptr = m.as_ptr();
            let code_ptr = m
                .as_ptr()
                .offset(num_code_pages as isize * page_size() as isize);
            eprintln!(
                "Memory Block Created: Code: {:#08x}, Data: {:#08x}",
                code_ptr as usize, data_ptr as usize
            );
            code_heap.init(code_ptr as *mut u8, ps * num_code_pages);
            assert_eq!(code_heap.bottom(), code_ptr as *mut u8);
            data_heap.init(data_ptr as *mut u8, ps * num_data_pages);
            assert_eq!(data_heap.bottom(), data_ptr as *mut u8);
        }

        Ok(Self(Arc::new(Mutex::new(BlockFactoryInner {
            page_size: ps,
            code: code_heap,
            data: data_heap,
            m,
        }))))
    }

    pub fn alloc_data(&self, data_size: usize) -> Option<WritableDataBlock> {
        assert!(data_size > 0);
        let aligned_size = page_align(data_size, page_size());
        let layout = Layout::from_size_align(aligned_size, 16).unwrap();
        let p = self
            .0
            .as_ref()
            .lock()
            .unwrap()
            .data
            .allocate_first_fit(layout)
            .unwrap();

        eprintln!(
            "alloc data: {:#08x}, {}, {}",
            p.as_ptr() as usize,
            data_size,
            aligned_size
        );

        Some(WritableDataBlock(BlockInner {
            layout,
            size: data_size,
            p: Some(p),
            factory: self.clone(),
        }))
    }

    pub fn alloc_code(&self, data_size: usize) -> Option<WritableCodeBlock> {
        assert!(data_size > 0);
        let aligned_size = page_align(data_size, page_size());
        let layout = Layout::from_size_align(aligned_size, 16).unwrap();
        let p = self
            .0
            .as_ref()
            .lock()
            .unwrap()
            .code
            .allocate_first_fit(layout)
            .unwrap();
        eprintln!(
            "alloc code: {:#08x}, {}, {}",
            p.as_ptr() as usize,
            data_size,
            aligned_size
        );
        Some(WritableCodeBlock(BlockInner {
            layout,
            size: data_size,
            p: Some(p),
            factory: self.clone(),
        }))
    }

    fn deallocate_code(&self, block: &BlockInner) {
        if let Some(ptr) = block.p {
            eprintln!(
                "Freeing Code at {:#08x}+{:x}",
                ptr.as_ptr() as usize,
                block.layout.size()
            );
            unsafe {
                block
                    .factory
                    .0
                    .as_ref()
                    .lock()
                    .unwrap()
                    .code
                    .deallocate(ptr, block.layout);
            }
        }
    }

    fn deallocate_data(&self, block: &BlockInner) {
        if let Some(ptr) = block.p {
            eprintln!(
                "Freeing Data at {:#08x}+{:x}",
                ptr.as_ptr() as usize,
                block.layout.size()
            );
            unsafe {
                block
                    .factory
                    .0
                    .as_ref()
                    .lock()
                    .unwrap()
                    .data
                    .deallocate(ptr, block.layout);
            }
        }
    }

    fn deallocate(&self, block: &Block) {
        match block {
            Block::DataRW(WritableDataBlock(b)) | Block::DataRO(ReadonlyDataBlock(b)) => {
                self.deallocate_data(b)
            }
            Block::CodeRW(WritableCodeBlock(b)) | Block::CodeRX(ExecutableCodeBlock(b)) => {
                self.deallocate_code(b)
            }
        }
    }

    pub fn debug(&self) {
        self.0.as_ref().lock().unwrap().debug();
    }
}

impl BlockFactoryInner {
    pub fn debug(&self) {
        let ps = page_size();
        let code_size = self.code.size();
        let data_size = self.data.size();
        eprintln!("Start: {:#08x}", self.m.as_ptr() as usize);
        eprintln!("Code Bottom: {:#08x}", self.code.bottom() as usize);
        eprintln!("Code Top: {:#08x}", self.code.top() as usize);
        eprintln!("Code Size: {:#08x} ({})", code_size, code_size);
        eprintln!("Data Bottom: {:#08x}", self.data.bottom() as usize);
        eprintln!("Data Top: {:#08x}", self.data.top() as usize);
        eprintln!("Data Size: {:#08x} ({})", data_size, data_size);
        eprintln!("Page Size: {:#08x} ({})", ps, ps);
    }
}

pub enum Block {
    DataRW(WritableDataBlock),
    DataRO(ReadonlyDataBlock),
    CodeRW(WritableCodeBlock),
    CodeRX(ExecutableCodeBlock),
}

pub enum DataBlock {
    RW(WritableDataBlock),
    RO(ReadonlyDataBlock),
}

#[derive(Debug)]
pub struct ReadonlyDataBlock(pub(crate) BlockInner);
impl ReadonlyDataBlock {
    pub fn as_ptr(&self) -> *const u8 {
        self.0.p.unwrap().as_ptr() as *const u8
    }
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

#[derive(Debug)]
pub struct WritableDataBlock(pub(crate) BlockInner);
impl WritableDataBlock {
    pub fn as_ptr(&self) -> *mut u8 {
        self.0.p.unwrap().as_ptr()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_mut_slice(&self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn make_readonly_data(mut self) -> io::Result<ReadonlyDataBlock> {
        self.0.make_read_only()?;
        let p = self.0.p.take();
        Ok(ReadonlyDataBlock(BlockInner {
            layout: self.0.layout,
            size: self.0.size,
            p,
            factory: self.0.factory.clone(),
        }))
    }
}

pub enum CodeBlock {
    RW(WritableCodeBlock),
    RX(ExecutableCodeBlock),
}

#[derive(Debug)]
pub struct WritableCodeBlock(pub(crate) BlockInner);
impl WritableCodeBlock {
    pub fn as_ptr(&self) -> *mut u8 {
        self.0.p.unwrap().as_ptr()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_mut_slice(&self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn make_exec(&mut self) -> io::Result<ExecutableCodeBlock> {
        self.0.make_exec()?;
        let p = self.0.p.take();
        eprintln!("make exec: {:#08x}", p.unwrap().as_ptr() as usize);
        Ok(ExecutableCodeBlock(BlockInner {
            layout: self.0.layout,
            size: self.0.size,
            p,
            factory: self.0.factory.clone(),
        }))
    }
}

#[derive(Debug)]
pub struct ExecutableCodeBlock(pub BlockInner);
impl ExecutableCodeBlock {
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0.p.unwrap().as_ptr() as *const u8
    }
}

pub struct BlockInner {
    layout: Layout,
    pub(crate) size: usize,
    p: Option<NonNull<u8>>,
    factory: BlockFactory,
}

impl fmt::Debug for BlockInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Block").field("p", &self.p).finish()
    }
}

impl BlockInner {
    pub fn as_slice(&self) -> &[u8] {
        let ptr = self.p.unwrap().as_ptr();
        let size = self.layout.size();
        unsafe { std::slice::from_raw_parts(ptr, size) }
    }

    pub fn as_mut_slice(&self) -> &mut [u8] {
        let ptr = self.p.unwrap().as_ptr();
        let size = self.layout.size();
        unsafe { std::slice::from_raw_parts_mut(ptr, size) }
    }

    fn mprotect(&mut self, prot: libc::c_int) -> io::Result<()> {
        unsafe {
            let alignment = self.p.unwrap().as_ptr() as usize % page_size();
            let ptr = self.p.unwrap().as_ptr().offset(-(alignment as isize));
            let len = self.layout.size() + alignment;
            eprintln!("mprotect: {:#08x}+{:x}: {:x}", ptr as usize, len, prot);
            if libc::mprotect(ptr as *mut libc::c_void, len, prot) == 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        }
    }

    fn make_read_only(&mut self) -> io::Result<()> {
        self.mprotect(libc::PROT_READ)
    }

    fn make_exec(&mut self) -> io::Result<()> {
        self.mprotect(libc::PROT_READ | libc::PROT_EXEC)
    }

    fn make_mut(&mut self) -> io::Result<()> {
        self.mprotect(libc::PROT_READ | libc::PROT_WRITE)
    }
}

impl Drop for ReadonlyDataBlock {
    fn drop(&mut self) {
        // we need to make it mutable again before deallocating
        // because the allocator needs to make some changes
        if self.0.p.is_some() {
            self.0.make_mut().unwrap();
        }
        self.0.factory.deallocate_data(&self.0);
    }
}

impl Drop for WritableDataBlock {
    fn drop(&mut self) {
        if self.0.p.is_some() {
            self.0.make_mut().unwrap();
        }
        self.0.factory.deallocate_data(&self.0);
    }
}

impl Drop for WritableCodeBlock {
    fn drop(&mut self) {
        if self.0.p.is_some() {
            self.0.make_mut().unwrap();
        }
        self.0.factory.deallocate_code(&self.0);
    }
}

impl Drop for ExecutableCodeBlock {
    fn drop(&mut self) {
        self.0.make_mut();
        self.0.factory.deallocate_code(&self.0);
    }
}

fn page_align(n: usize, ps: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    return (n + (ps - 1)) & !(ps - 1);
}

fn page_size() -> usize {
    unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate() {
        let b = BlockFactory::create(2, 2).unwrap();
        let v1 = b.alloc_code(10).unwrap();
        let v2 = b.alloc_code(10).unwrap();
        b.debug();
        eprintln!("V Size: {:#08x}", v1.as_ptr() as usize);
        eprintln!("V Size: {:#08x}", v2.as_ptr() as usize);
        drop(v1);
        drop(v2);
        let v3 = b.alloc_code(10).unwrap();
        eprintln!("V Size: {:#08x}", v3.as_ptr() as usize);
    }
}
