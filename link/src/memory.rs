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
    heap: Heap,
}

impl BlockFactoryInner {
    pub fn force_rw(&mut self) {
        self.mprotect(libc::PROT_READ | libc::PROT_WRITE);
    }

    fn mprotect(&mut self, prot: libc::c_int) -> io::Result<()> {
        unsafe {
            let alignment = self.m.as_ptr() as usize % page_size();
            let ptr = self.m.as_ptr().offset(-(alignment as isize));
            let len = self.m.len() + alignment;
            eprintln!("mprotect: {:#08x}+{:x}: {:x}", ptr as usize, len, prot);
            if libc::mprotect(ptr as *mut libc::c_void, len, prot) == 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        }
    }
}

impl BlockFactory {
    pub fn get_mem_ptr(&self) -> (*const u8, usize) {
        let m = &self.0.lock().unwrap().m;
        (m.as_ptr(), m.len())
    }

    pub fn force_rw(&mut self) {
        self.0.lock().unwrap().force_rw();
    }

    pub fn create(num_pages: usize) -> Result<BlockFactory, Box<dyn Error>> {
        // the total amount of space allocated should not be more than 4GB,
        // because we are limited to 32bit relative addressing
        // we can address things outside this block, but we need 64 bit addressing
        let ps = page_size();
        let size = ps * num_pages;
        let m = MmapMut::map_anon(size)?;
        //unsafe {
        //libc::mprotect(m.as_ptr() as *mut libc::c_void, size_plus_metadata, 7);
        //}
        let mut heap = Heap::empty();

        unsafe {
            let ptr = m.as_ptr();
            eprintln!("Memory Block Created: {:#08x}+{:x}", ptr as usize, size);
            heap.init(ptr as *mut u8, ps * num_pages);
            assert_eq!(heap.bottom(), ptr as *mut u8);
        }

        Ok(Self(Arc::new(Mutex::new(BlockFactoryInner {
            page_size: ps,
            heap,
            m,
        }))))
    }

    pub fn alloc_block(&self, size: usize) -> Option<Block> {
        assert!(size > 0);
        let aligned_size = page_align(size, page_size());
        let layout = Layout::from_size_align(aligned_size, 16).unwrap();
        let p = self
            .0
            .as_ref()
            .lock()
            .unwrap()
            .heap
            .allocate_first_fit(layout)
            .unwrap();

        eprintln!(
            "Block Allocate: Addr: {:#08x}, size: {}, aligned_size: {}",
            p.as_ptr() as usize,
            size,
            aligned_size
        );

        Some(Block {
            layout,
            size,
            p: Some(p),
            factory: self.clone(),
        })
    }

    fn deallocate_block(&self, block: &Block) {
        if let Some(ptr) = block.p {
            eprintln!(
                "Freeing Block at {:#08x}+{:x}",
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
                    .heap
                    .deallocate(ptr, block.layout);
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
        let size = self.heap.size();
        eprintln!("Start: {:#08x}", self.m.as_ptr() as usize);
        eprintln!("Heap Bottom: {:#08x}", self.heap.bottom() as usize);
        eprintln!("Heap Top: {:#08x}", self.heap.top() as usize);
        eprintln!("Heap Size: {:#08x} ({})", size, size);
        eprintln!("Page Size: {:#08x} ({})", ps, ps);
    }
}

pub struct SmartPointer {
    layout: Layout,
    p: NonNull<u8>,
    block: HeapBlock,
}

impl SmartPointer {
    pub fn as_ptr(&self) -> *const u8 {
        self.p.as_ptr() as *const u8
    }
    pub fn copy(&mut self, buf: &[u8]) {
        // copy data into the area
        assert!(buf.len() <= self.layout.size());
        let size = std::cmp::min(buf.len(), self.layout.size());
        unsafe {
            std::ptr::copy(buf.as_ptr(), self.p.as_ptr(), size);
        }
    }
}

impl Drop for SmartPointer {
    fn drop(&mut self) {
        self.block.clone().free(self); //0.lock().unwrap().free(self);
    }
}

pub struct HeapBlockInner {
    block: Block,
    heap: Heap,
}

#[derive(Clone)]
pub struct HeapBlock(Arc<Mutex<HeapBlockInner>>);

impl HeapBlock {
    pub fn new(block: Block) -> Self {
        unsafe {
            let mut heap = Heap::empty();
            heap.init(block.as_mut_ptr(), block.layout.size());
            assert_eq!(heap.bottom(), block.as_mut_ptr());
            Self(Arc::new(Mutex::new(HeapBlockInner { block, heap })))
        }
    }

    pub fn base(&self) -> *const () {
        self.0.as_ref().lock().unwrap().block.as_ptr() as *const ()
    }

    pub fn add_buf(&mut self, buf: &[u8]) -> Option<SmartPointer> {
        match self.alloc(buf.len()) {
            Some(mut p) => {
                p.copy(buf);
                Some(p)
            }
            None => None,
        }
    }

    pub fn alloc(&mut self, size: usize) -> Option<SmartPointer> {
        assert!(size > 0);
        let layout = Layout::from_size_align(size, 1).unwrap();
        let p = self
            .0
            .lock()
            .unwrap()
            .heap
            .allocate_first_fit(layout)
            .unwrap();

        eprintln!("alloc: {:#08x}, {}", p.as_ptr() as usize, size,);

        Some(SmartPointer {
            layout,
            p,
            block: self.clone(),
        })
    }

    fn free(&mut self, pointer: &SmartPointer) {
        eprintln!(
            "Freeing Pointer at {:#08x}+{:x}",
            pointer.as_ptr() as usize,
            pointer.layout.size()
        );
        unsafe {
            self.0
                .lock()
                .unwrap()
                .heap
                .deallocate(pointer.p, pointer.layout);
        }
    }
}

pub struct Block {
    layout: Layout,
    pub(crate) size: usize,
    p: Option<NonNull<u8>>,
    factory: BlockFactory,
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Block").field("p", &self.p).finish()
    }
}

impl Block {
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

    pub fn as_ptr(&self) -> *const u8 {
        self.p.unwrap().as_ptr() as *const u8
    }

    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.p.unwrap().as_ptr() as *mut u8
    }

    pub fn make_readonly_block(mut self) -> io::Result<Self> {
        self.make_read_only()?;
        let p = self.p.take();
        Ok(Block {
            layout: self.layout,
            size: self.size,
            p,
            factory: self.factory.clone(),
        })
    }

    pub fn make_exec_block(mut self) -> io::Result<Self> {
        self.make_exec()?;
        let p = self.p.take();
        eprintln!("make exec: {:#08x}", p.unwrap().as_ptr() as usize);
        Ok(Block {
            layout: self.layout,
            size: self.size,
            p,
            factory: self.factory.clone(),
        })
    }

    pub fn make_heap_block(self) -> HeapBlock {
        HeapBlock::new(self)
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

impl Drop for Block {
    fn drop(&mut self) {
        // we need to make it mutable again before deallocating
        // because the allocator needs to make some changes
        if self.p.is_some() {
            self.make_mut().unwrap();
        }
        self.factory.deallocate_block(&self);
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
        let b = BlockFactory::create(2).unwrap();
        let v1 = b.alloc_block(10).unwrap();
        let v2 = b.alloc_block(10).unwrap();
        b.debug();
        eprintln!("V Size: {:#08x}", v1.as_ptr() as usize);
        eprintln!("V Size: {:#08x}", v2.as_ptr() as usize);
        drop(v1);
        drop(v2);
        let v3 = b.alloc_block(10).unwrap();
        eprintln!("V Size: {:#08x}", v3.as_ptr() as usize);
    }

    #[test]
    fn heapblock() {
        let b = BlockFactory::create(2).unwrap();
        let mut h = b.alloc_block(1).unwrap().make_heap_block();
        let v1 = h.alloc(10).unwrap();
        let v2 = h.alloc(10).unwrap();
        eprintln!("V Size: {:#08x}", v1.as_ptr() as usize);
        eprintln!("V Size: {:#08x}", v2.as_ptr() as usize);
        drop(v1);
        drop(v2);
        let v3 = h.alloc(10).unwrap();
        eprintln!("V Size: {:#08x}", v3.as_ptr() as usize);
        b.debug();
    }
}