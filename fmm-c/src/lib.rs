mod error;

pub use error::*;
use fmm::ir::*;
use indoc::indoc;

const INCLUDES: &str = indoc! {"
    #include <stdatomic.h>
    #include <stdbool.h>
    #include <stdlib.h>
"};

pub fn compile(_module: &Module) -> Result<String, CompileError> {
    Ok(INCLUDES.into())
}
