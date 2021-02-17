mod cps_transformer;

use crate::ir::*;
use cps_transformer::*;

pub fn transform_to_cps(module: &Module) -> Module {
    CpsTransformer::new().transform(module)
}
