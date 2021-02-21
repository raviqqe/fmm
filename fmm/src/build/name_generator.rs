#[derive(Clone, Debug, Default)]
pub struct NameGenerator {
    prefix: String,
    index: u64,
}

impl NameGenerator {
    pub fn new(prefix: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            index: 0,
        }
    }

    pub fn generate(&mut self) -> String {
        let name = format!("{}{:x}", &self.prefix, self.index);

        self.index += 1;

        name
    }
}
