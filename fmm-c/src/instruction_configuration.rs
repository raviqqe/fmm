#[cfg(test)]
use once_cell::sync::Lazy;

#[derive(Clone, Debug)]
pub struct InstructionConfiguration {
    pub allocate_function_name: String,
    pub reallocate_function_name: String,
    pub free_function_name: String,
}

#[cfg(test)]
pub static DUMMY_INSTRUCTION_CONFIGURATION: Lazy<InstructionConfiguration> =
    Lazy::new(|| InstructionConfiguration {
        allocate_function_name: "my_malloc".into(),
        reallocate_function_name: "my_realloc".into(),
        free_function_name: "my_free".into(),
    });
