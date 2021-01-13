use super::names::*;
use super::typed_expression::*;
use crate::ir::*;
use crate::types::{self, Type};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct BuildState {
    instructions: Vec<Instruction>,
}

impl BuildState {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn allocate_heap(&mut self, type_: impl Into<Type>) -> TypedExpression {
        let name = generate_name();
        let type_ = type_.into();

        self.instructions
            .push(AllocateHeap::new(type_.clone(), &name).into());

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_))
    }

    pub fn arithmetic_operation(
        &mut self,
        operator: ArithmeticOperator,
        lhs: impl Into<TypedExpression>,
        rhs: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let type_ = lhs.type_().to_primitive().unwrap();
        let name = generate_name();

        self.instructions.push(
            ArithmeticOperation::new(
                type_,
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn atomic_load(&mut self, pointer: impl Into<TypedExpression>) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer.type_().to_pointer().unwrap().element().clone();
        let name = generate_name();

        self.instructions
            .push(AtomicLoad::new(type_.clone(), pointer.expression().clone(), &name).into());

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn atomic_store(
        &mut self,
        value: impl Into<TypedExpression>,
        pointer: impl Into<TypedExpression>,
    ) {
        let value = value.into();
        let pointer = pointer.into();

        self.instructions.push(
            AtomicStore::new(
                value.type_().clone(),
                value.expression().clone(),
                pointer.expression().clone(),
            )
            .into(),
        );
    }

    pub fn call(
        &mut self,
        function: impl Into<TypedExpression>,
        arguments: impl IntoIterator<Item = TypedExpression>,
    ) -> TypedExpression {
        let function = function.into();
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let type_ = function.type_().to_function().unwrap().clone();
        let name = generate_name();

        self.instructions.push(
            Call::new(
                type_.clone(),
                function.expression().clone(),
                arguments
                    .iter()
                    .map(|typed_expression| typed_expression.expression())
                    .cloned()
                    .collect(),
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), type_.result().clone())
    }

    pub fn compare_and_swap(
        &mut self,
        pointer: impl Into<TypedExpression>,
        old_value: impl Into<TypedExpression>,
        new_value: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let old_value = old_value.into();
        let new_value = new_value.into();
        let name = generate_name();

        self.instructions.push(
            CompareAndSwap::new(
                old_value.type_().clone(),
                pointer.expression().clone(),
                old_value.expression().clone(),
                new_value.expression().clone(),
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), types::Primitive::Bool)
    }

    pub fn comparison_operation(
        &mut self,
        operator: ComparisonOperator,
        lhs: impl Into<TypedExpression>,
        rhs: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let name = generate_name();

        self.instructions.push(
            ComparisonOperation::new(
                lhs.type_().to_primitive().unwrap(),
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), types::Primitive::Bool)
    }

    pub fn deconstruct_record(
        &mut self,
        record: impl Into<TypedExpression>,
        element_index: usize,
    ) -> TypedExpression {
        let record = record.into();
        let type_ = record.type_().to_record().unwrap().clone();
        let name = generate_name();

        self.instructions.push(
            DeconstructRecord::new(
                type_.clone(),
                record.expression().clone(),
                element_index,
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), type_.elements()[element_index].clone())
    }

    pub fn deconstruct_union(
        &mut self,
        union: impl Into<TypedExpression>,
        member_index: usize,
    ) -> TypedExpression {
        let union = union.into();
        let type_ = union.type_().to_union().unwrap().clone();
        let name = generate_name();

        self.instructions.push(
            DeconstructUnion::new(
                type_.clone(),
                union.expression().clone(),
                member_index,
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), type_.members()[member_index].clone())
    }

    pub fn load(&mut self, pointer: impl Into<TypedExpression>) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer.type_().to_pointer().unwrap().element().clone();
        let name = generate_name();

        self.instructions
            .push(Load::new(type_.clone(), pointer.expression().clone(), &name).into());

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn pointer_address(
        &mut self,
        pointer: impl Into<TypedExpression>,
        offset: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let offset = offset.into();
        let type_ = pointer.type_().to_pointer().unwrap().clone();
        let name = generate_name();

        self.instructions.push(
            PointerAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                offset.expression().clone(),
                &name,
            )
            .into(),
        );

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn record_address(
        &mut self,
        pointer: impl Into<TypedExpression>,
        element_index: usize,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer
            .type_()
            .to_pointer()
            .unwrap()
            .element()
            .to_record()
            .unwrap()
            .clone();
        let name = generate_name();

        self.instructions.push(
            RecordAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                element_index,
                &name,
            )
            .into(),
        );

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(type_.elements()[element_index].clone()),
        )
    }

    pub fn store(
        &mut self,
        value: impl Into<TypedExpression>,
        pointer: impl Into<TypedExpression>,
    ) {
        let value = value.into();
        let pointer = pointer.into();

        self.instructions.push(
            Store::new(
                value.type_().clone(),
                value.expression().clone(),
                pointer.expression().clone(),
            )
            .into(),
        );
    }

    pub fn union_address(
        &mut self,
        pointer: impl Into<TypedExpression>,
        member_index: usize,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer
            .type_()
            .to_pointer()
            .unwrap()
            .element()
            .to_union()
            .unwrap()
            .clone();
        let name = generate_name();

        self.instructions.push(
            UnionAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                member_index,
                &name,
            )
            .into(),
        );

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(type_.members()[member_index].clone()),
        )
    }

    pub fn branch(&self, typed_expression: impl Into<TypedExpression>) -> Block {
        let typed_expression = typed_expression.into();

        Block::new(
            self.instructions.iter().cloned(),
            Branch::new(
                typed_expression.type_().clone(),
                typed_expression.expression().clone(),
            ),
        )
    }

    pub fn return_(&self, typed_expression: impl Into<TypedExpression>) -> Block {
        let typed_expression = typed_expression.into();

        Block::new(
            self.instructions.iter().cloned(),
            Return::new(
                typed_expression.type_().clone(),
                typed_expression.expression().clone(),
            ),
        )
    }

    pub fn unreachable(&self) -> Block {
        Block::new(vec![], TerminalInstruction::Unreachable)
    }
}
