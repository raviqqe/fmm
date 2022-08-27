#[cfg(test)]
use once_cell::sync::Lazy;

#[derive(Clone)]
pub struct InstructionConfiguration {
    pub allocate_function_name: String,
    pub reallocate_function_name: String,
    pub free_function_name: String,
    pub unreachable_function_name: Option<String>,
}

pub struct InstructionFunctionSet<'c> {
    pub allocate_function: inkwell::values::FunctionValue<'c>,
    pub reallocate_function: inkwell::values::FunctionValue<'c>,
    pub free_function: inkwell::values::FunctionValue<'c>,
    pub unreachable_function: Option<inkwell::values::FunctionValue<'c>>,
}

#[cfg(test)]
pub static DUMMY_INSTRUCTION_CONFIGURATION: Lazy<InstructionConfiguration> =
    Lazy::new(|| InstructionConfiguration {
        allocate_function_name: "my_malloc".into(),
        reallocate_function_name: "my_realloc".into(),
        free_function_name: "my_free".into(),
        unreachable_function_name: None,
    });
