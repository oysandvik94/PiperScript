use bytecode::{Instructions, OpCode};

use crate::{
    eval::objects::PrimitiveObject,
    parser::{
        ast::{Operator, Statement},
        expressions::expression::Expression,
    },
};

pub mod bytecode;
pub mod internal_error;

#[derive(Default)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<PrimitiveObject>,
}

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<PrimitiveObject>,
}

impl Compiler {
    fn emit(&mut self, op_code: OpCode, operands: &[u16]) -> usize {
        let instruction = bytecode::make(op_code, operands);
        self.add_instruction(&instruction)
    }

    pub fn compile(&mut self, ast: Vec<Statement>) {
        for node in ast {
            match node {
                Statement::Assign(_) => todo!(),
                Statement::Return(_) => todo!(),
                Statement::Expression(expression) => self.compile_expression(&expression),
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Infix {
                left,
                right,
                operator,
            } => self.compile_infix_expersion(left.as_ref(), right.as_ref(), operator),
            Expression::StringLiteral(_) => todo!(),
            Expression::IntegerLiteral(integer) => {
                let integer_object = PrimitiveObject::Integer(*integer);
                let constant_index = self.add_constant(integer_object);

                self.emit(OpCode::OpConstant, &[constant_index as u16]);
            }
            Expression::BooleanLiteral(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::HashLiteral(_) => todo!(),
            Expression::Index { left: _, index: _ } => todo!(),
            Expression::Prefix {
                right: _,
                operator: _,
            } => todo!(),
            Expression::If(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::IdentifierLiteral(_) => todo!(),
        }
    }

    fn compile_infix_expersion(
        &mut self,
        left: &Expression,
        right: &Expression,
        operator: &Operator,
    ) {
        self.compile_expression(left);
        self.compile_expression(right);

        match operator {
            Operator::Bang => todo!(),
            Operator::Minus => todo!(),
            Operator::Plus => self.emit(OpCode::Add, &[]),
            Operator::Multiply => todo!(),
            Operator::Equals => todo!(),
            Operator::NotEquals => todo!(),
            Operator::GreaterThan => todo!(),
            Operator::LessThan => todo!(),
            Operator::DividedBy => todo!(),
        };
    }

    fn add_constant(&mut self, object: PrimitiveObject) -> usize {
        self.constants.push(object);
        self.constants.len() - 1
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_instruction(&mut self, instruction: &[u8]) -> usize {
        let new_instruction_position = self.instructions.0.len();
        self.instructions.0.extend_from_slice(instruction);
        new_instruction_position
    }
}

#[cfg(test)]
mod tests {
    use bytecode::OpCode;

    use crate::test_util::{self, CompilerTestCase};

    use super::*;

    #[test]
    fn test_integer_arithmetic() {
        let test_cases: Vec<CompilerTestCase> = vec![CompilerTestCase {
            input: String::from("1 + 2"),
            expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
            expected_instructions: vec![
                Instructions(bytecode::make(OpCode::OpConstant, &[0])),
                Instructions(bytecode::make(OpCode::OpConstant, &[1])),
                Instructions(bytecode::make(OpCode::Add, &[])),
            ],
        }];

        test_util::run_compiler_tests(test_cases);
    }
}
