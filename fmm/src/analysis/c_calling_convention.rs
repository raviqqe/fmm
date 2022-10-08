use super::type_check::{self, TypeCheckError};
use crate::ir::*;

pub fn transform(module: &Module) -> Result<Module, TypeCheckError> {
    type_check::check(module)?;

    let module = Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module.function_definitions().to_vec(),
    );

    type_check::check(&module)?;

    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform(&Module::new(vec![], vec![], vec![], vec![])),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }
}
