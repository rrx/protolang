use linked_list_allocator::*;
use memmap::*;
use std::error::Error;
use std::sync::{Mutex, Arc};
use std::alloc::Layout;
use std::ptr::NonNull;
use std::io;

#[derive(Clone)]
pub struct BlockFactory(Arc<Mutex<BlockFactoryInner>>);

pub struct BlockFactoryInner {
    m: MmapMut,
    code: Heap,
    data: Heap,
}

impl BlockFactory {
    pub fn create(num_code_pages: usize, num_data_pages: usize) -> Result<BlockFactory, Box<dyn Error>> {
        // the total amount of space allocated should not be more than 4GB,
        // because we are limited to 32bit relative addressing
        // we can address things outside this block, but we need 64 bit addressing
        let ps = page_size();
        let size_plus_metadata = ps * (num_code_pages + num_data_pages);
        let m = MmapMut::map_anon(size_plus_metadata)?;
        let mut data_heap = Heap::empty();
        let mut code_heap = Heap::empty();


        unsafe {
            let data_ptr = m.as_ptr();
            let code_ptr = m.as_ptr().offset(num_code_pages as isize * page_size() as isize);
            data_heap.init(data_ptr as *mut u8, ps * num_data_pages);
            code_heap.init(code_ptr as *mut u8, ps * num_code_pages);
        }

        Ok(Self(Arc::new(Mutex::new(BlockFactoryInner {
            code: code_heap,
            data: data_heap,
            m
        }))))
    }

    pub fn alloc_data(&self, size: usize) -> Option<DataBlock> {
        let layout = Layout::from_size_align(size, 16).unwrap();
        let p = self.0.as_ref().lock().unwrap().data.allocate_first_fit(layout).unwrap();
        Some(DataBlock(Block {
            layout,
            p: Some(p),
            factory: self.clone()
        }))
    }

    pub fn alloc_code(&self, size: usize) -> Option<CodeBlock> {
        let layout = Layout::from_size_align(size, 16).unwrap();
        let p = self.0.as_ref().lock().unwrap().code.allocate_first_fit(layout).unwrap();
        Some(CodeBlock(Block {
            layout,
            p: Some(p),
            factory: self.clone()
        }))
    }

    fn deallocate_code(&self, block: &Block) {
        if let Some(ptr) = block.p {
            println!("Freeing Code at {:#08x}", ptr.as_ptr() as usize);
            unsafe {
                block.factory.0.as_ref().lock().unwrap().code.deallocate(ptr, block.layout);
            }
        }
    }

    fn deallocate_data(&self, block: &Block) {
        if let Some(ptr) = block.p {
            println!("Freeing Data at {:#08x}", ptr.as_ptr() as usize);
            unsafe {
                block.factory.0.as_ref().lock().unwrap().data.deallocate(ptr, block.layout);
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

pub struct ReadonlyDataBlock(Block);
impl ReadonlyDataBlock {
    pub fn as_ptr(&self) -> *const u8 {
        self.0.p.unwrap().as_ptr() as *const u8
    }
}

pub struct DataBlock(Block);
impl DataBlock {
    pub fn as_ptr(&self) -> *mut u8 {
        self.0.p.unwrap().as_ptr()
    }
    pub fn make_readonly_data(mut self) -> io::Result<ReadonlyDataBlock> {
        self.0.make_read_only()?;
        let p = self.0.p.take();
        Ok(ReadonlyDataBlock(Block {
            layout: self.0.layout,
            p,
            factory: self.0.factory.clone()
        }))
    }
}

pub struct WritableCodeBlock(Block);
impl WritableCodeBlock {
    pub fn as_ptr(&self) -> *mut u8 {
        self.0.p.unwrap().as_ptr()
    }

    pub fn make_exec(&mut self) -> io::Result<CodeBlock> {
        self.0.make_exec()?;
        let p = self.0.p.take();
        Ok(CodeBlock(Block {
            layout: self.0.layout,
            p,
            factory: self.0.factory.clone()
        }))
    }
}

pub struct CodeBlock(Block);
impl CodeBlock {
    pub fn as_ptr(&self) -> *const u8 {
        self.0.p.unwrap().as_ptr() as *const u8 
    }
}

pub struct Block {
    layout: Layout,
    p: Option<NonNull<u8>>,
    factory: BlockFactory
}

impl Block {
    fn mprotect(&mut self, prot: libc::c_int) -> io::Result<()> {
        unsafe {
            let alignment = self.p.unwrap().as_ptr() as usize % page_size();
            let ptr = self.p.unwrap().as_ptr().offset(-(alignment as isize));
            let len = self.layout.size() + alignment;
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
        self.0.factory.deallocate_data(&self.0);
    }
}

impl Drop for DataBlock {
    fn drop(&mut self) {
        self.0.factory.deallocate_data(&self.0);
    }
}

impl Drop for WritableCodeBlock {
    fn drop(&mut self) {
        self.0.factory.deallocate_code(&self.0);
    }
}

impl Drop for CodeBlock {
    fn drop(&mut self) {
        self.0.factory.deallocate_code(&self.0);
    }
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

