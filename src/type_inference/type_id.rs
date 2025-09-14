#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl From<TypeId> for usize {
    fn from(value: TypeId) -> Self {
        value.0 as usize
    }
}

impl From<usize> for TypeId {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl TypeId {
    pub const ERROR: TypeId = TypeId(0);
    pub const UNIT: TypeId = TypeId(1);
    pub const BOOL: TypeId = TypeId(2);
    pub const POINTER: TypeId = TypeId(3);
    pub const CHAR: TypeId = TypeId(4);
    pub const STRING: TypeId = TypeId(5);

    pub const I8: TypeId = TypeId(6);
    pub const I16: TypeId = TypeId(7);
    pub const I32: TypeId = TypeId(8);
    pub const I64: TypeId = TypeId(9);
    pub const ISZ: TypeId = TypeId(10);

    pub const U8: TypeId = TypeId(11);
    pub const U16: TypeId = TypeId(12);
    pub const U32: TypeId = TypeId(13);
    pub const U64: TypeId = TypeId(14);
    pub const USZ: TypeId = TypeId(15);

    pub const F32: TypeId = TypeId(16);
    pub const F64: TypeId = TypeId(17);
}
