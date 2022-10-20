use crate::ir::Module;

pub fn validate(module: &Module) -> Result<(), Validation> {
    name::check(module)?;
    type_check::check(module)?;

    Ok(())
}

pub enum ValidateError {
    Name(NameError),
    TypeCheck(TypeCheckError),
}
