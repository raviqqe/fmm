use crate::{build::NameGenerator, types::Type};
use std::{cell::RefCell, rc::Rc};

pub struct Context {
    name_generator: Rc<RefCell<NameGenerator>>,
    result_type: Type,
}

impl Context {
    pub fn new(result_type: impl Into<Type>) -> Self {
        Self {
            name_generator: Rc::new(NameGenerator::new("_k_").into()),
            result_type: result_type.into(),
        }
    }

    pub fn name_generator(&self) -> Rc<RefCell<NameGenerator>> {
        self.name_generator.clone()
    }

    pub fn result_type(&self) -> &Type {
        &self.result_type
    }
}
