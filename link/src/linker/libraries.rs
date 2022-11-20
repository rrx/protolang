use std::ffi::CString;
use std::sync::Arc;

pub type SharedLibrary = Arc<libloading::Library>;

#[derive(Clone)]
pub struct SharedLibraryRepo {
    map: im::HashMap<String, Arc<libloading::Library>>,
}

impl SharedLibraryRepo {
    pub fn add(&mut self, name: &str, lib: libloading::Library) {
        self.map.insert(name.to_string(), Arc::new(lib));
    }

    // search the dynamic libraries to see if the symbol exists
    pub fn search_dynamic(&self, symbol: &str) -> Option<*const ()> {
        for (_name, lib) in &self.map {
            let cstr = CString::new(symbol).unwrap();
            unsafe {
                let result: Result<libloading::Symbol<unsafe fn()>, libloading::Error> =
                    lib.get(cstr.as_bytes());
                if let Ok(f) = result {
                    return Some(f.into_raw().into_raw() as *const ());
                }
            }
        }
        None
    }
}

impl Default for SharedLibraryRepo {
    fn default() -> Self {
        Self {
            map: im::HashMap::new(),
        }
    }
}
