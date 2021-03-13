pub struct HeapConfiguration {
    pub allocate_function_name: String,
    pub reallocate_function_name: String,
}

pub struct HeapFunctionSet<'c> {
    pub allocate_function: inkwell::values::FunctionValue<'c>,
    pub reallocate_function: inkwell::values::FunctionValue<'c>,
}
