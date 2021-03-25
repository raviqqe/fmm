use super::name_generator::NameGenerator;
use super::typed_expression::*;
use crate::ir::*;
use crate::types::{self, Type};
use std::cell::RefCell;
use std::rc::Rc;

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

    pub fn allocate_heap(&self, type_: impl Into<Type>) -> TypedExpression {
        let name = self.generate_name();
        let type_ = type_.into();

        self.add_instruction(AllocateHeap::new(type_.clone(), &name));

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_))
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

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(types::Primitive::Integer8),
        )
    }

    pub fn allocate_stack(&self, type_: impl Into<Type>) -> TypedExpression {
        let name = self.generate_name();
        let type_ = type_.into();

        self.add_instruction(AllocateStack::new(type_.clone(), &name));

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_))
    }

    pub fn arithmetic_operation(
        &self,
        operator: ArithmeticOperator,
        lhs: impl Into<TypedExpression>,
        rhs: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let type_ = lhs.type_().to_primitive().unwrap();
        let name = self.generate_name();

        self.add_instruction(ArithmeticOperation::new(
            type_,
            operator,
            lhs.expression().clone(),
            rhs.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn atomic_load(&self, pointer: impl Into<TypedExpression>) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer.type_().to_pointer().unwrap().element().clone();
        let name = self.generate_name();

        self.add_instruction(AtomicLoad::new(
            type_.clone(),
            pointer.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn atomic_operation(
        &self,
        operator: AtomicOperator,
        pointer: impl Into<TypedExpression>,
        value: impl Into<TypedExpression>,
    ) {
        let value = value.into();
        let pointer = pointer.into();

        self.add_instruction(AtomicOperation::new(
            value.type_().to_primitive().unwrap(),
            operator,
            pointer.expression().clone(),
            value.expression().clone(),
        ));
    }

    pub fn atomic_store(
        &self,
        value: impl Into<TypedExpression>,
        pointer: impl Into<TypedExpression>,
    ) {
        let value = value.into();
        let pointer = pointer.into();

        self.add_instruction(AtomicStore::new(
            value.type_().clone(),
            value.expression().clone(),
            pointer.expression().clone(),
        ));
    }

    pub fn call(
        &self,
        function: impl Into<TypedExpression>,
        arguments: Vec<TypedExpression>,
    ) -> TypedExpression {
        let function = function.into();
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let type_ = function.type_().to_function().unwrap().clone();
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

        TypedExpression::new(Variable::new(name), type_.result().clone())
    }

    pub fn compare_and_swap(
        &self,
        pointer: impl Into<TypedExpression>,
        old_value: impl Into<TypedExpression>,
        new_value: impl Into<TypedExpression>,
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
            &name,
        ));

        TypedExpression::new(Variable::new(name), types::Primitive::Boolean)
    }

    pub fn comparison_operation(
        &self,
        operator: ComparisonOperator,
        lhs: impl Into<TypedExpression>,
        rhs: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let name = self.generate_name();

        self.add_instruction(ComparisonOperation::new(
            lhs.type_().to_primitive().unwrap(),
            operator,
            lhs.expression().clone(),
            rhs.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), types::Primitive::Boolean)
    }

    pub fn deconstruct_record(
        &self,
        record: impl Into<TypedExpression>,
        element_index: usize,
    ) -> TypedExpression {
        let record = record.into();
        let type_ = record.type_().to_record().unwrap().clone();
        let name = self.generate_name();

        self.add_instruction(DeconstructRecord::new(
            type_.clone(),
            record.expression().clone(),
            element_index,
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_.elements()[element_index].clone())
    }

    pub fn deconstruct_union(
        &self,
        union: impl Into<TypedExpression>,
        member_index: usize,
    ) -> TypedExpression {
        let union = union.into();
        let type_ = union.type_().to_union().unwrap().clone();
        let name = self.generate_name();

        self.add_instruction(DeconstructUnion::new(
            type_.clone(),
            union.expression().clone(),
            member_index,
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_.members()[member_index].clone())
    }

    pub fn if_(
        &self,
        condition: impl Into<TypedExpression>,
        then: impl Fn(Self) -> Block,
        else_: impl Fn(Self) -> Block,
    ) -> TypedExpression {
        let condition = condition.into();
        let then = then(self.clone_empty());
        let else_ = else_(self.clone_empty());

        let name = self.generate_name();
        let type_ = if let Some(branch) = then.terminal_instruction().to_branch() {
            branch.type_().clone()
        } else if let Some(branch) = else_.terminal_instruction().to_branch() {
            branch.type_().clone()
        } else {
            types::Record::new(vec![]).into()
        };

        self.add_instruction(If::new(
            type_.clone(),
            condition.expression().clone(),
            then,
            else_,
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn load(&self, pointer: impl Into<TypedExpression>) -> TypedExpression {
        let pointer = pointer.into();
        let type_ = pointer.type_().to_pointer().unwrap().element().clone();
        let name = self.generate_name();

        self.add_instruction(Load::new(
            type_.clone(),
            pointer.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn pass_through(&self, value: impl Into<TypedExpression>) -> TypedExpression {
        let value = value.into();
        let name = self.generate_name();

        self.add_instruction(PassThrough::new(
            value.type_().clone(),
            value.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), value.type_().clone())
    }

    pub fn pointer_address(
        &self,
        pointer: impl Into<TypedExpression>,
        offset: impl Into<TypedExpression>,
    ) -> TypedExpression {
        let pointer = pointer.into();
        let offset = offset.into();
        let type_ = pointer.type_().to_pointer().unwrap().clone();
        let name = self.generate_name();

        self.add_instruction(PointerAddress::new(
            type_.clone(),
            pointer.expression().clone(),
            offset.expression().clone(),
            &name,
        ));

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn record_address(
        &self,
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
        let name = self.generate_name();

        self.add_instruction(RecordAddress::new(
            type_.clone(),
            pointer.expression().clone(),
            element_index,
            &name,
        ));

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(type_.elements()[element_index].clone()),
        )
    }

    pub fn store(&self, value: impl Into<TypedExpression>, pointer: impl Into<TypedExpression>) {
        let value = value.into();
        let pointer = pointer.into();

        self.add_instruction(Store::new(
            value.type_().clone(),
            value.expression().clone(),
            pointer.expression().clone(),
        ));
    }

    pub fn union_address(
        &self,
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
        let name = self.generate_name();

        self.add_instruction(UnionAddress::new(
            type_.clone(),
            pointer.expression().clone(),
            member_index,
            &name,
        ));

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(type_.members()[member_index].clone()),
        )
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

    fn add_instruction(&self, instruction: impl Into<Instruction>) {
        self.instructions.borrow_mut().push(instruction.into());
    }

    fn generate_name(&self) -> String {
        self.name_generator.borrow_mut().generate()
    }
}
