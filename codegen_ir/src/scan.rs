use crate::hir::*;
use crate::visit;

struct DeclarationScan {
    definitions: Vec<Definition>,
}

impl DeclarationScan {
    fn new() -> Self {
        Self {
            definitions: vec![],
        }
    }
}

// record the definitions, so we can refer to them later
// we need this so we can call variables that might be defined later
impl visit::Visitor<()> for DeclarationScan {
    fn enter_definition(&mut self, d: &Definition, _: &mut ()) -> visit::VResult {
        match &*d.expr {
            Ast::Lambda(_) => self.definitions.push(d.clone()),
            _ => unimplemented!(),
        }
        Ok(())
    }
}

pub fn scan_definitions(ast: &Ast) -> Vec<Definition> {
    let mut scan = DeclarationScan::new();
    visit::visit(ast, &mut scan, &mut ()).unwrap();
    scan.definitions
}
