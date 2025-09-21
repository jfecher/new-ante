use std::collections::BTreeMap;

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::{
    iterator_extensions::vecmap,
    lexer::token::{FloatKind, IntegerKind},
    type_inference::{
        type_id::TypeId,
        types::{FunctionType, PrimitiveType, Type},
    },
    vecmap::VecMap,
};

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct TypeContext {
    id_to_type: VecMap<TypeId, Type>,
    type_to_id: BTreeMap<Type, TypeId>,
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContext {
    /// Create a new type context, initially populated with primitive
    /// types. These are pre-populated so we can refer to statically-known
    /// ids instead of requiring a lookup whenever constructing a primitive type.
    pub fn new() -> Self {
        let mut id_to_type = VecMap::default();
        let mut type_to_id = FxHashMap::default();
        let mut insert = |typ| {
            let id = id_to_type.push(Type::Primitive(typ));
            type_to_id.insert(id, Type::Primitive(typ));
            id
        };

        let error = insert(PrimitiveType::Error);
        let unit = insert(PrimitiveType::Unit);
        let bool = insert(PrimitiveType::Bool);
        let pointer = insert(PrimitiveType::Pointer);
        let char = insert(PrimitiveType::Char);
        let string = insert(PrimitiveType::String);
        let pair = insert(PrimitiveType::Pair);
        let i8 = insert(PrimitiveType::Int(IntegerKind::I8));
        let i16 = insert(PrimitiveType::Int(IntegerKind::I16));
        let i32 = insert(PrimitiveType::Int(IntegerKind::I32));
        let i64 = insert(PrimitiveType::Int(IntegerKind::I64));
        let isz = insert(PrimitiveType::Int(IntegerKind::Isz));
        let u8 = insert(PrimitiveType::Int(IntegerKind::U8));
        let u16 = insert(PrimitiveType::Int(IntegerKind::U16));
        let u32 = insert(PrimitiveType::Int(IntegerKind::U32));
        let u64 = insert(PrimitiveType::Int(IntegerKind::U64));
        let usz = insert(PrimitiveType::Int(IntegerKind::Usz));
        let f32 = insert(PrimitiveType::Float(FloatKind::F32));
        let f64 = insert(PrimitiveType::Float(FloatKind::F64));

        assert_eq!(error, TypeId::ERROR);
        assert_eq!(unit, TypeId::UNIT);
        assert_eq!(bool, TypeId::BOOL);
        assert_eq!(pointer, TypeId::POINTER);
        assert_eq!(char, TypeId::CHAR);
        assert_eq!(string, TypeId::STRING);
        assert_eq!(pair, TypeId::PAIR);
        assert_eq!(i8, TypeId::I8);
        assert_eq!(i16, TypeId::I16);
        assert_eq!(i32, TypeId::I32);
        assert_eq!(i64, TypeId::I64);
        assert_eq!(isz, TypeId::ISZ);
        assert_eq!(u8, TypeId::U8);
        assert_eq!(u16, TypeId::U16);
        assert_eq!(u32, TypeId::U32);
        assert_eq!(u64, TypeId::U64);
        assert_eq!(usz, TypeId::USZ);
        assert_eq!(f32, TypeId::F32);
        assert_eq!(f64, TypeId::F64);

        Self { id_to_type, type_to_id: Default::default() }
    }

    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.id_to_type[id]
    }

    pub fn get_or_insert_type(&mut self, typ: Type) -> TypeId {
        if let Some(id) = self.type_to_id.get(&typ) {
            return *id;
        }

        let next_id = self.id_to_type.push(typ.clone());
        self.type_to_id.insert(typ, next_id);
        next_id
    }

    /// Convert an ast type to a TypeId as closely as possible.
    /// This method does not emit any errors and relies on name resolution
    /// to emit errors when resolving types.
    pub fn convert_ast_type(&mut self, typ: &crate::parser::cst::Type) -> TypeId {
        match typ {
            crate::parser::cst::Type::Integer(kind) => match kind {
                IntegerKind::I8 => TypeId::I8,
                IntegerKind::I16 => TypeId::I16,
                IntegerKind::I32 => TypeId::I32,
                IntegerKind::I64 => TypeId::I64,
                IntegerKind::Isz => TypeId::ISZ,
                IntegerKind::U8 => TypeId::U8,
                IntegerKind::U16 => TypeId::U16,
                IntegerKind::U32 => TypeId::U32,
                IntegerKind::U64 => TypeId::U64,
                IntegerKind::Usz => TypeId::USZ,
            },
            crate::parser::cst::Type::Float(kind) => match kind {
                FloatKind::F32 => TypeId::F32,
                FloatKind::F64 => TypeId::F64,
            },
            crate::parser::cst::Type::String => TypeId::STRING,
            crate::parser::cst::Type::Char => TypeId::CHAR,
            crate::parser::cst::Type::Named(_path) => todo!("Resolve named types"),
            crate::parser::cst::Type::Variable(_name) => todo!("Resolve named types"),
            crate::parser::cst::Type::Function(function) => {
                let parameters = vecmap(&function.parameters, |typ| self.convert_ast_type(typ));
                let return_type = self.convert_ast_type(&function.return_type);
                // TODO: Effects
                let effects = TypeId::UNIT;
                let typ = Type::Function(FunctionType { parameters, return_type, effects });
                self.get_or_insert_type(typ)
            },
            crate::parser::cst::Type::Error => TypeId::ERROR,
            crate::parser::cst::Type::Unit => TypeId::UNIT,
            crate::parser::cst::Type::Application(f, args) => {
                let f = self.convert_ast_type(f);
                let args = vecmap(args, |typ| self.convert_ast_type(typ));
                let typ = Type::Application(f, args);
                self.get_or_insert_type(typ)
            },
            crate::parser::cst::Type::Reference(mutability, sharedness) => {
                let typ = Type::Reference(*mutability, *sharedness);
                self.get_or_insert_type(typ)
            },
        }
    }
}
