pub struct Context {
    word_bytes: usize,
}

impl Context {
    pub fn new(word_bytes: usize) -> Self {
        Self { word_bytes }
    }

    pub fn word_bytes(&self) -> usize {
        self.word_bytes
    }
}
