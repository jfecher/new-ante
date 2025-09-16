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

    pub fn integer(kind: crate::lexer::token::IntegerKind) -> TypeId {
        match kind {
            crate::lexer::token::IntegerKind::I8 => TypeId::I8,
            crate::lexer::token::IntegerKind::I16 => TypeId::I16,
            crate::lexer::token::IntegerKind::I32 => TypeId::I32,
            crate::lexer::token::IntegerKind::I64 => TypeId::I64,
            crate::lexer::token::IntegerKind::Isz => TypeId::ISZ,
            crate::lexer::token::IntegerKind::U8 => TypeId::U8,
            crate::lexer::token::IntegerKind::U16 => TypeId::U16,
            crate::lexer::token::IntegerKind::U32 => TypeId::U32,
            crate::lexer::token::IntegerKind::U64 => TypeId::U64,
            crate::lexer::token::IntegerKind::Usz => TypeId::USZ,
        }
    }

    pub fn float(kind: crate::lexer::token::FloatKind) -> TypeId {
        match kind {
            crate::lexer::token::FloatKind::F32 => TypeId::F32,
            crate::lexer::token::FloatKind::F64 => TypeId::F64,
        }
    }
}
