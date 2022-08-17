use super::atomic_ordering::AtomicOrdering;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fence {
    ordering: AtomicOrdering,
}

impl Fence {
    pub fn new(ordering: AtomicOrdering) -> Self {
        Self { ordering }
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.ordering
    }
}
