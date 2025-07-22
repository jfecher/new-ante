use serde::{Deserialize, Serialize};

use crate::parser::ids::TopLevelId;

#[derive(Clone)]
pub(super) enum Namespace {
    /// A module within a crate
    Module(CrateId, ModuleId),

    /// A type's namespace containing its methods
    Type(TopLevelId),
}

impl Namespace {
    pub(super) fn crate_(crate_id: CrateId) -> Self {
        Namespace::Module(crate_id, CRATE_ROOT_MODULE)
    }
}

/// A crate's id is a hash of its name and its version.
/// Crate ids are expected to be globally unique.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct CrateId(u32);

/// A crate's id is a hash of its path from the crate root.
/// Module ids are expected to be unique only within the same crate.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ModuleId(u32);

/// A crate's root module always has ID 0
pub const CRATE_ROOT_MODULE: ModuleId = ModuleId(0);
