use std::sync::atomic::{AtomicU64, Ordering};

static NAME_ID: AtomicU64 = AtomicU64::new(0);

pub fn generate_name() -> String {
    format!("_fmm_{:x}", NAME_ID.fetch_add(1, Ordering::SeqCst))
}
