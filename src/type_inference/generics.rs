use serde::{Deserialize, Serialize};

use crate::{parser::ids::NameId, type_inference::types::TypeVariableId};

#[derive(Copy, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Generic {
    Name(NameId),
    Inferred(TypeVariableId),
}
