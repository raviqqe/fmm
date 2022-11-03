#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArgumentOptions {
    alias: bool,
}

impl ArgumentOptions {
    pub fn new() -> Self {
        Self { alias: true }
    }

    pub fn alias(&self) -> bool {
        self.alias
    }

    pub fn set_alias(self, alias: bool) -> Self {
        Self { alias, ..self }
    }
}

impl Default for ArgumentOptions {
    fn default() -> Self {
        Self::new()
    }
}
