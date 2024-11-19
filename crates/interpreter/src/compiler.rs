use bytecode::Instruction;

use crate::{
    eval::objects::PrimitiveObject,
    parser::{
        ast::{Operator, PrefixOperator, Statement, StatementType},
        expressions::expression::Expression,
    },
};

pub mod bytecode;
pub mod internal_error;

#[derive(Default)]
pub struct Compiler {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<PrimitiveObject>,
}

pub struct ByteCode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<PrimitiveObject>,
}

impl Compiler {
    pub fn compile(&mut self, ast: Vec<Statement>) {
        for node in ast {
            match node.statement_type {
                StatementType::Assign(_) => todo!(),
                StatementType::Return(_) => todo!(),
                StatementType::Expression(expression) => {
                    self.compile_expression(&expression);
                    self.add_instruction(Instruction::Pop);
                }
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

                self.add_instruction(Instruction::OpConstant(constant_index as u16));
            }
            Expression::BooleanLiteral(value) => {
                _ = self.add_instruction(match value {
                    true => Instruction::True,
                    false => Instruction::False,
                })
            }
            Expression::Array(_) => todo!(),
            Expression::HashLiteral(_) => todo!(),
            Expression::Index { left: _, index: _ } => todo!(),
            Expression::Prefix { right, operator } => {
                self.compile_expression(right);

                match operator {
                    PrefixOperator::Bang => {
                        self.add_instruction(Instruction::Bang);
                    }
                    PrefixOperator::Minus => {
                        self.add_instruction(Instruction::Minus);
                    }
                }
            }
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
        match operator {
            Operator::Bang => todo!(),
            Operator::Minus => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::Sub)
            }
            Operator::Plus => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::Add)
            }
            Operator::Multiply => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::Mul)
            }
            Operator::Equals => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::Equal)
            }
            Operator::NotEquals => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::NotEqual)
            }
            Operator::GreaterThan => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::GreaterThan)
            }
            Operator::LessThan => {
                self.compile_expression(right);
                self.compile_expression(left);
                self.add_instruction(Instruction::GreaterThan)
            }
            Operator::DividedBy => {
                self.compile_left_right(left, right);
                self.add_instruction(Instruction::Div)
            }
        };
    }

    fn compile_left_right(&mut self, left: &Expression, right: &Expression) {
        self.compile_expression(left);
        self.compile_expression(right);
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

    fn add_instruction(&mut self, instruction: Instruction) -> usize {
        let new_instruction_position = self.instructions.len();
        self.instructions.push(instruction);
        new_instruction_position
    }
}

#[cfg(test)]
mod tests {

    use crate::test_util::{self, CompilerTestCase};

    use super::*;

    #[test]
    fn test_integer_arithmetic() {
        let test_cases: Vec<CompilerTestCase> = vec![
            CompilerTestCase {
                input: String::from("1"),
                expected_constants: vec![PrimitiveObject::Integer(1)],
                expected_instructions: vec![Instruction::OpConstant(0), Instruction::Pop],
            },
            CompilerTestCase {
                input: String::from("1 + 2"),
                expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::Add,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("5 - 3"),
                expected_constants: vec![PrimitiveObject::Integer(5), PrimitiveObject::Integer(3)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::Sub,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("1 * 2"),
                expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::Mul,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("2 / 1"),
                expected_constants: vec![PrimitiveObject::Integer(2), PrimitiveObject::Integer(1)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::Div,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("-1"),
                expected_constants: vec![PrimitiveObject::Integer(1)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::Minus,
                    Instruction::Pop,
                ],
            },
        ];

        test_util::run_compiler_tests(test_cases);
    }

    #[test]
    fn test_boolean_expressions() {
        let test_cases: Vec<CompilerTestCase> = vec![
            CompilerTestCase {
                input: String::from("true"),
                expected_constants: vec![],
                expected_instructions: vec![Instruction::True, Instruction::Pop],
            },
            CompilerTestCase {
                input: String::from("false"),
                expected_constants: vec![],
                expected_instructions: vec![Instruction::False, Instruction::Pop],
            },
            CompilerTestCase {
                input: String::from("1 > 2"),
                expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::GreaterThan,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("1 < 2"),
                expected_constants: vec![PrimitiveObject::Integer(2), PrimitiveObject::Integer(1)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::GreaterThan,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("1 == 2"),
                expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::Equal,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("1 != 2"),
                expected_constants: vec![PrimitiveObject::Integer(1), PrimitiveObject::Integer(2)],
                expected_instructions: vec![
                    Instruction::OpConstant(0),
                    Instruction::OpConstant(1),
                    Instruction::NotEqual,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("true == false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::False,
                    Instruction::Equal,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("true != false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::False,
                    Instruction::NotEqual,
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("!true"),
                expected_constants: vec![],
                expected_instructions: vec![Instruction::True, Instruction::Bang, Instruction::Pop],
            },
        ];

        test_util::run_compiler_tests(test_cases);
    }
}
