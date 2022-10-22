use crate::build::NameGenerator;
use std::{cell::RefCell, rc::Rc};

pub struct Context {
    name_generator: Rc<RefCell<NameGenerator>>,
    word_bytes: usize,
}

impl Context {
    pub fn new(word_bytes: usize) -> Self {
        Self {
            name_generator: Rc::new(NameGenerator::new("_c_").into()),
            word_bytes,
        }
    }

    pub fn name_generator(&self) -> Rc<RefCell<NameGenerator>> {
        self.name_generator.clone()
    }

    pub fn word_bytes(&self) -> usize {
        self.word_bytes
    }
}
