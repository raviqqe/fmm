use super::{
    error::BuildError, expressions::variable, name_generator::NameGenerator, typed_expression::*,
};
use crate::{
    ir::*,
    types::{self, generic_pointer_type, void_type, Type},
};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug, Default)]
pub struct InstructionBuilder {
    name_generator: Rc<RefCell<NameGenerator>>,
    instructions: RefCell<Vec<Instruction>>,
}

impl InstructionBuilder {
    pub fn new(name_generator: Rc<RefCell<NameGenerator>>) -> Self {
        Self {
            name_generator,
            instructions: vec![].into(),
        }
    }

    fn clone_empty(&self) -> Self {
        Self::new(self.name_generator.clone())
    }

    pub fn allocate_heap(&self, size: impl Into<TypedExpression>) -> TypedExpression {
        let name = self.generate_name();

        self.add_instruction(AllocateHeap::new(size.into().expression().clone(), &name));

        TypedExpression::new(Variable::new(name), generic_pointer_type())
    }

    pub fn reallocate_heap(
        &self,
        pointer: impl Into<TypedExpression>,
        size: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let name = self.generate_name();

        self.add_instruction(ReallocateHeap::new(
            pointer.into().expression().clone(),
            size.into().expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), generic_pointer_type())
    }

    pub fn allocate_stack(&self, type_: impl Into<Type>) -> TypedExpression {
        let name = self.generate_name();
        let type_ = type_.into();

        self.add_instruction(AllocateStack::new(type_.clone(), &name));

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_))
    }

    pub fn atomic_load(
        &self,
        pointer: impl Into<TypedExpression>,
        ordering: AtomicOrdering,
    ) -> Result<TypedExpression, BuildError> {
        let pointer = pointer.into();
        let type_ = pointer
            .type_()
            .to_pointer()
            .ok_or_else(|| BuildError::PointerExpected(pointer.type_().clone()))?
            .element()
            .clone();
        let name = self.generate_name();

        self.add_instruction(AtomicLoad::new(
            type_.clone(),
            pointer.expression().clone(),
            ordering,
            &name,
        ));

        Ok(TypedExpression::new(Variable::new(name), type_))
    }

    pub fn atomic_operation(
        &self,
        operator: AtomicOperator,
        pointer: impl Into<TypedExpression>,
        value: impl Into<TypedExpression>,
        ordering: AtomicOrdering,
    ) -> Result<TypedExpression, BuildError> {
        let pointer = pointer.into();
        let value = value.into();
        let type_ = value
            .type_()
            .to_primitive()
            .ok_or_else(|| BuildError::PrimitiveExpected(value.type_().clone()))?;
        let name = self.generate_name();

        self.add_instruction(AtomicOperation::new(
            type_,
            operator,
            pointer.expression().clone(),
            value.expression().clone(),
            ordering,
            &name,
        ));

        Ok(variable(name, type_))
    }

    pub fn atomic_store(
        &self,
        value: impl Into<TypedExpression>,
        pointer: impl Into<TypedExpression>,
        ordering: AtomicOrdering,
    ) {
        let value = value.into();
        let pointer = pointer.into();

        self.add_instruction(AtomicStore::new(
            value.type_().clone(),
            value.expression().clone(),
            pointer.expression().clone(),
            ordering,
        ));
    }

    pub fn call(
        &self,
        function: impl Into<TypedExpression>,
        arguments: Vec<TypedExpression>,
    ) -> Result<TypedExpression, BuildError> {
        let function = function.into();
        let type_ = function
            .type_()
            .to_function()
            .ok_or_else(|| BuildError::FunctionExpected(function.type_().clone()))?
            .clone();
        let name = self.generate_name();

        self.add_instruction(Call::new(
            type_.clone(),
            function.expression().clone(),
            arguments
                .iter()
                .map(|typed_expression| typed_expression.expression())
                .cloned()
                .collect(),
            &name,
        ));

        Ok(variable(name, type_.result().clone()))
    }

    pub fn compare_and_swap(
        &self,
        pointer: impl Into<TypedExpression>,
        old_value: impl Into<TypedExpression>,
        new_value: impl Into<TypedExpression>,
        success_ordering: AtomicOrdering,
        failure_ordering: AtomicOrdering,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let old_value = old_value.into();
        let new_value = new_value.into();
        let name = self.generate_name();

        self.add_instruction(CompareAndSwap::new(
            old_value.type_().clone(),
            pointer.expression().clone(),
            old_value.expression().clone(),
            new_value.expression().clone(),
            success_ordering,
            failure_ordering,
            &name,
        ));

        TypedExpression::new(Variable::new(name), types::Primitive::Boolean)
    }

    pub fn deconstruct_record(
        &self,
        record: impl Into<TypedExpression>,
        field_index: usize,
    ) -> Result<TypedExpression, BuildError> {
        let record = record.into();
        let type_ = record
            .type_()
            .to_record()
            .ok_or_else(|| BuildError::RecordExpected(record.type_().clone()))?
            .clone();
        let name = self.generate_name();

        self.add_instruction(DeconstructRecord::new(
            type_.clone(),
            record.expression().clone(),
            field_index,
            &name,
        ));

        Ok(variable(name, type_.fields()[field_index].clone()))
    }

    pub fn deconstruct_union(
        &self,
        union: impl Into<TypedExpression>,
        member_index: usize,
    ) -> Result<TypedExpression, BuildError> {
        let union = union.into();
        let type_ = union
            .type_()
            .to_union()
            .ok_or_else(|| BuildError::UnionExpected(union.type_().clone()))?
            .clone();
        let name = self.generate_name();

        self.add_instruction(DeconstructUnion::new(
            type_.clone(),
            union.expression().clone(),
            member_index,
            &name,
        ));

        Ok(variable(name, type_.members()[member_index].clone()))
    }

    pub fn fence(&self, ordering: AtomicOrdering) {
        self.add_instruction(Fence::new(ordering))
    }

    pub fn free_heap(&self, pointer: impl Into<TypedExpression>) {
        self.add_instruction(FreeHeap::new(pointer.into().expression().clone()));
    }

    pub fn if_<E>(
        &self,
        condition: impl Into<TypedExpression>,
        then: impl Fn(Self) -> Result<Block, E>,
        else_: impl Fn(Self) -> Result<Block, E>,
    ) -> Result<TypedExpression, E> {
        let condition = condition.into();
        let then = then(self.clone_empty())?;
        let else_ = else_(self.clone_empty())?;

        let name = self.generate_name();
        let type_ = if let Some(branch) = then.terminal_instruction().to_branch() {
            branch.type_().clone()
        } else if let Some(branch) = else_.terminal_instruction().to_branch() {
            branch.type_().clone()
        } else {
            void_type().into()
        };

        self.add_instruction(If::new(
            type_.clone(),
            condition.expression().clone(),
            then,
            else_,
            &name,
        ));

        Ok(variable(name, type_))
    }

    pub fn load(&self, pointer: impl Into<TypedExpression>) -> Result<TypedExpression, BuildError> {
        let pointer = pointer.into();
        let type_ = pointer
            .type_()
            .to_pointer()
            .ok_or_else(|| BuildError::PointerExpected(pointer.type_().clone()))?
            .element()
            .clone();
        let name = self.generate_name();

        self.add_instruction(Load::new(
            type_.clone(),
            pointer.expression().clone(),
            &name,
        ));

        Ok(TypedExpression::new(Variable::new(name), type_))
    }

    pub fn memory_copy(
        &self,
        source: impl Into<TypedExpression>,
        destination: impl Into<TypedExpression>,
        size: impl Into<TypedExpression>,
    ) {
        self.add_instruction(MemoryCopy::new(
            source.into().expression().clone(),
            destination.into().expression().clone(),
            size.into().expression().clone(),
        ));
    }

    pub fn store(&self, value: impl Into<TypedExpression>, pointer: impl Into<TypedExpression>) {
        let value = value.into();

        self.add_instruction(Store::new(
            value.type_().clone(),
            value.expression().clone(),
            pointer.into().expression().clone(),
        ));
    }

    pub fn branch(&self, typed_expression: impl Into<TypedExpression>) -> Block {
        let typed_expression = typed_expression.into();

        Block::new(
            self.instructions.borrow().clone(),
            Branch::new(
                typed_expression.type_().clone(),
                typed_expression.expression().clone(),
            ),
        )
    }

    pub fn return_(&self, typed_expression: impl Into<TypedExpression>) -> Block {
        let typed_expression = typed_expression.into();

        Block::new(
            self.instructions.borrow().clone(),
            Return::new(
                typed_expression.type_().clone(),
                typed_expression.expression().clone(),
            ),
        )
    }

    pub fn unreachable(&self) -> Block {
        Block::new(
            self.instructions.borrow().clone(),
            TerminalInstruction::Unreachable,
        )
    }

    pub fn into_instructions(self) -> Vec<Instruction> {
        self.instructions.into_inner()
    }

    pub fn add_instruction(&self, instruction: impl Into<Instruction>) {
        self.instructions.borrow_mut().push(instruction.into());
    }

    fn generate_name(&self) -> String {
        self.name_generator.borrow_mut().generate()
    }
}
