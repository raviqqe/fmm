#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AtomicOrdering {
    Relaxed,
    Acquire,
    Release,
    AcquireRelease,
    SequentiallyConsistent,
}
