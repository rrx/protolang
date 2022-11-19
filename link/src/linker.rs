use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::CString;
use std::path::Path;
use std::sync::{Arc, Mutex};

use std::fs;

use super::*;

#[derive(Clone, Debug)]
pub struct LinkedBlock(Arc<LinkedBlockInner>);
impl LinkedBlock {
    pub fn disassemble(&self) {
        //let pointers = im::HashMap::new();
        let inner = &self.0.as_ref();
        match inner {
            LinkedBlockInner::Code(block) => block.disassemble(),//disassemble_code(block.as_slice(), pointers),
            LinkedBlockInner::DataRO(block) => block.disassemble(),//disassemble_data(block.as_slice(), pointers),
            LinkedBlockInner::DataRW(block) => block.disassemble()//disassemble_data(block.as_slice(), pointers),
        }
    }
}
#[derive(Debug)]
pub enum LinkedBlockInner {
    Code(ExecutableCodeBlock),
    DataRO(ReadonlyDataBlock),
    DataRW(WritableDataBlock),
}


pub type PatchSymbolPointers = im::HashMap<String, *const ()>;
pub type LinkedSymbolPointers = im::HashMap<String, *const ()>;

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

pub fn patch_block(
    r: &CodeRelocation,
    patch_base: *mut u8,
    pointers: &im::HashMap<String, *const ()>,
) {
    use object::RelocationKind;
    println!("{}", r);
    match r.r.kind {
        RelocationKind::Elf(42) => {
            // got entry + addend - reloc_offset(patch)
            // we are computing the offset from the current instruction pointer
            unsafe {
                //let patch_base = block.block.as_ptr() as *mut u8;
                let patch = patch_base.offset(r.offset as isize);

                // get the entry in the lookup table
                let addr = *pointers.get(&r.name).unwrap() as *const u8;

                // this works
                let value = addr as isize + r.r.addend as isize - patch as isize;

                // this does not work
                //let value = patch as isize + rel.r.addend as isize - addr as isize;

                let before = std::ptr::read(patch);
                (patch as *mut u32).replace(value as u32);
                println!("patch_base: {:#08x}", patch_base as usize);
                println!("patch: {:#08x}", patch as usize);
                println!("value: {:#04x}", value as u32);

                println!(
                    "rel got {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                    &r.name, patch as usize, before, value as u32, r.r.addend, addr as usize,
                );
            }
        }

        RelocationKind::Absolute => {
            // S + A
            // S = Address of the symbol
            // A = value of the Addend
            //
            // We get this if we don't compile with -fPIC
            // This doesn't work, and produces an illegal address for some reason
            let name = &r.name;
            println!("look up: {}", name);
            unsafe {
                //let patch_base = block.block.as_ptr() as *mut u8;

                // address of remote
                let addr = *pointers.get(name).unwrap() as *const u8;
                let adjusted = addr as isize + r.r.addend as isize;

                let (before, patch) = match r.r.size {
                    32 => {
                        // patch as 32 bit
                        let patch = patch_base.offset(r.offset as isize) as *mut u32;
                        let before = std::ptr::read(patch);
                        patch.replace(adjusted as u32);
                        (before as u64, patch as u64)
                    }
                    64 => {
                        // patch as 64 bit
                        let patch = patch_base.offset(r.offset as isize) as *mut u64;
                        let before = std::ptr::read(patch);
                        patch.replace(adjusted as u64);
                        (before as u64, patch as u64)
                    }
                    _ => unimplemented!(),
                };

                println!(
                    "rel absolute {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                    name, patch, before, adjusted as u32, r.r.addend, addr as u32,
                );
            }
        }
        RelocationKind::PltRelative => {
            // L + A - P, 32 bit output
            // L = address of the symbols entry within the procedure linkage table
            // A = value of the Addend
            // P = address of the place of the relocation

            let name = &r.name;
            println!("look up: {}", name);
            let addend = r.r.addend;

            // complicated pointer arithmetic to update the relocations
            //
            unsafe {
                //let patch_base = block.block.as_ptr() as *mut u8;
                let patch = patch_base.offset(r.offset as isize);

                eprintln!("look up: {}",  name);
                let symbol_addr = *pointers.get(name).unwrap() as *const u8;
                let symbol_address = symbol_addr as isize + addend as isize - patch as isize;

                // patch as 32 bit
                let patch = patch as *mut u32;
                patch.replace(symbol_address as u32);

                println!(
                        "rel {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                        name,
                        patch as usize,
                        std::ptr::read(patch),
                        addend,
                        symbol_addr as isize,
                        symbol_address as isize,
                        );
            }
        }
        _ => unimplemented!(),
    }
}

pub fn patch_code(
    mut block: PatchCodeBlock,
    pointers: &PatchSymbolPointers,
) -> (LinkedBlock, LinkedSymbolPointers) {
    println!(
        "patching code {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );
    for (_name, r) in &block.relocations {
        let patch_base = block.block.as_ptr();
        patch_block(r, patch_base, pointers);
    }
    (
        LinkedBlock(Arc::new(LinkedBlockInner::Code(
            block.block.make_exec().unwrap(),
        ))),
        block.symbols,
    )
    //(None, None)
}

pub fn patch_data(
    block: PatchDataBlock,
    pointers: &PatchSymbolPointers,
) -> (LinkedBlock, LinkedSymbolPointers) {
    println!(
        "patching data {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );
    for (_name, r) in &block.relocations {
        let patch_base = block.block.as_ptr();
        patch_block(r, patch_base, pointers);
    }
    (
        LinkedBlock(Arc::new(LinkedBlockInner::DataRW(block.block))),
        block.symbols,
    )
}

#[derive(Clone)]
pub struct LinkVersion {
    linked: im::HashMap<String, LinkedBlock>,
    pointers: LinkedSymbolPointers,
}

impl LinkVersion {
    pub fn new() -> Self {
        Self {
            linked: im::HashMap::new(),
            pointers: im::HashMap::new(),
        }
    }

    pub fn debug(&self) {
        for (k, v) in &self.linked {
            println!("link: {:?}", (&k, &v));
        }
    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function

        // make sure we dereference the pointer!
        let ptr = *self.pointers.get(name).ok_or(LinkError::SymbolNotFound)? as *const();
        unsafe {
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            println!("invoking {} @ {:#08x}", name, ptr as usize);
            let f: MyFunc<P, T> = std::mem::transmute(ptr);

            for map in proc_maps::get_process_maps(std::process::id() as proc_maps::Pid).unwrap() {
                println!("Map: {:#08x}+{:x}, {}, {:?}", map.start(), map.size(), map.flags, map.filename());
            }

            let ret = f(args);
            Ok(ret)
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

pub fn build_version(mut blocks: Vec<(String, PatchBlock)>, link: &Link) -> Result<LinkVersion, Box<dyn Error>> {
    // generate a list of symbols and their pointers
    let mut all_pointers = im::HashMap::new();

    for (block_name, block) in &blocks {

        // look up all of the unknowns in the shared libraries, if they exist
        if let PatchBlock::Code(PatchCodeBlock { unknowns, .. }) = block {
            for symbol in unknowns {
                if let Ok(Some(ptr)) = link.search_dynamic(symbol) {
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
        }
        match block {
            PatchBlock::Code(PatchCodeBlock { symbols, .. })
            | PatchBlock::Data(PatchDataBlock { symbols, .. }) => {
                for (symbol, ptr) in symbols {
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
        }
    }

    // patch everything
    let mut all_linked_pointers = im::HashMap::new();
    let mut linked = im::HashMap::new();
    for (block_name, mut block) in blocks {
        let (patched_block, linked_pointers) = block.patch(all_pointers.clone());
        patched_block.disassemble();
        linked.insert(block_name.clone(), patched_block);
        all_linked_pointers = all_linked_pointers.union(linked_pointers);
    }

    Ok(LinkVersion {
        linked,
        pointers: all_linked_pointers,
    })
}

pub struct Link {
    collection: LinkVersion,
    pages: HashMap<String, UnlinkedCodeSegment>,
    libraries: HashMap<String, libloading::Library>,
    mem: BlockFactory,
}

impl Link {
    pub fn new() -> Self {
        Self {
            collection: LinkVersion::new(),
            pages: HashMap::new(),
            libraries: HashMap::new(),
            mem: BlockFactory::create(10, 10).unwrap(),
        }
    }

    pub fn remove(&mut self, name: &str) {
        self.pages.remove(&name.to_string());
    }

    pub fn add_library(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        unsafe {
            let lib = libloading::Library::new(path)?;
            // we need to parse the header files to know what all of the symbols mean
            // but we may as well just use bindgen?
            //let _: libloading::Symbol<unsafe extern fn() -> u32> = lib.get(b"gzopen")?;
            self.libraries.insert(name.to_string(), lib);
            eprintln!("Loaded library: {}", &path.to_string_lossy());
        }
        Ok(())
    }

    fn add_segements(&mut self, segments: Vec<UnlinkedCodeSegmentInner>) {
        for s in segments {
            self.pages.insert(s.name.clone(), Arc::new(s));
        }
    }

    pub fn add_obj_file(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        let buf = fs::read(path)?;
        self.add_obj_buf(name, buf.as_slice())
    }

    pub fn add_obj_buf(&mut self, name: &str, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let segments = UnlinkedCodeSegmentInner::create_segments(name, buf)?;
        self.add_segements(segments);
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkVersion, Box<dyn Error>> {
        let mut pointers = im::HashMap::new();
        let mut duplicates = HashSet::new();

        // get all of the symbols and the name that provides it
        for (_name, unlinked) in &self.pages {
            //println!("linking: {}", name);
            for (symbol_name, code_symbol) in &unlinked.symbols {
                if code_symbol.def == CodeSymbolDefinition::Defined {
                    //println!("\tSymbol: {}", &symbol_name);
                    if pointers.contains_key(symbol_name) {
                        println!("\tDuplicate symbol: {}", &symbol_name);
                        duplicates.insert(symbol_name);
                    } else {
                        pointers.insert(symbol_name.clone(), code_symbol.address);
                    }
                }
            }
        }

        let mut relocations = HashSet::new();
        let mut missing = HashSet::new();
        for (_name, unlinked) in &self.pages {
            let mut children = HashSet::new();
            //println!("checking: {}", name);
            // ensure all relocations map somewhere
            for (symbol_name, _r) in &unlinked.relocations {
                //println!("\tReloc: {}", &symbol_name);
                if pointers.contains_key(symbol_name) || self.search_dynamic(symbol_name)?.is_some()
                {
                    children.insert(symbol_name.clone());
                    relocations.insert(symbol_name.clone());
                } else {
                    println!("\tSymbol {} missing", symbol_name);
                    missing.insert(symbol_name);
                }
            }
        }

        if missing.len() == 0 && duplicates.len() == 0 {
            let mut blocks = vec![];
            for (name, unlinked) in &self.pages {
                let name = format!("{}_data", &unlinked.name);
                if let Some(block) = unlinked.create_data_mem(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
                let name = format!("{}_code", &unlinked.name);
                if let Some(block) = unlinked.create_code_mem(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
            }
            build_version(blocks, &self)
        } else {
            Err(LinkError::MissingSymbol.into())
        }
    }

    // search the dynamic libraries to see if the symbol exists
    fn search_dynamic(&self, symbol: &str) -> Result<Option<*const ()>, Box<dyn Error>> {
        for (_name, lib) in &self.libraries {
            let cstr = CString::new(symbol)?;
            unsafe {
                let result: Result<libloading::Symbol<unsafe fn()>, libloading::Error> =
                    lib.get(cstr.as_bytes());
                if let Ok(f) = result {
                    return Ok(Some(f.into_raw().into_raw() as *const ()));
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn linker_global_long() {
        let mut b = Link::new();
        b.add_obj_file("test", Path::new("/home/rrx/code/protolang/tmp/live.o"))
            .unwrap();
        let collection = b.link().unwrap();
        
        let ret: i64 = collection.invoke("call_live", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(13, ret);

        let ret: i64 = collection.invoke("simple", ()).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(1, ret);

        let ret: i64 = collection.invoke("func2", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(3, ret);

    }

    #[test]
    fn linker_shared() {
        let mut b = Link::new();
        b.add_library("gz", Path::new("/home/rrx/code/protolang/tmp/libz.so"))
            .unwrap();
        b.add_obj_file(
            "test",
            Path::new("/home/rrx/code/protolang/tmp/link_shared.o"),
        )
        .unwrap();
        let collection = b.link().unwrap();
        let ret: std::ffi::c_void = collection.invoke("call_z", ()).unwrap();
        //println!("ret: {:#08x}", ret);
        //assert_eq!(3, ret);
    }


    #[test]
    fn linker_livelink() {
        let mut b = Link::new();

        // unable to link, missing symbol
        b.add_obj_file(
            "test1",
            Path::new("/home/rrx/code/protolang/tmp/testfunction.o"),
        )
        .unwrap();
        assert_eq!(false, b.link().is_ok());

        // provide missing symbol
        b.add_obj_file("asdf", Path::new("/home/rrx/code/protolang/tmp/asdf.o"))
            .unwrap();
        assert_eq!(true, b.link().is_ok());

        // links fine
        b.add_obj_file(
            "simple",
            Path::new("/home/rrx/code/protolang/tmp/simplefunction.o"),
        )
        .unwrap();
        assert_eq!(true, b.link().is_ok());

        let collection = b.link().unwrap();
        let ret: i64 = collection.invoke("func", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(10001, ret);

        let ret: i64 = collection.invoke("simple", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(10012, ret);

        let ret: i64 = collection.invoke("call_external", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(4, ret);

        let ret: i64 = collection.invoke("asdf", (2,)).unwrap();
        println!("ret: {}", ret);
        assert_eq!(3, ret);
    }
}
