use anyhow::Result;
use bytecode::Instruction;

use crate::{
    eval::objects::PrimitiveObject,
    parser::{
        ast::{BlockStatement, Operator, PrefixOperator, Statement, StatementType},
        expressions::{expression::Expression, if_expression},
    },
};

pub mod bytecode;
pub mod internal_error;

#[derive(Default)]
pub struct Compiler {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<PrimitiveObject>,
    pub last_instruction: Option<EmittedInstruction>,
    pub previous_instruction: Option<EmittedInstruction>,
}

pub struct ByteCode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<PrimitiveObject>,
}

pub struct EmittedInstruction {
    pub instruction: Instruction,
    pub position: usize,
}

impl Compiler {
    pub fn compile(&mut self, ast: Vec<Statement>) -> Result<()> {
        for node in ast {
            self.compile_statement(&node.statement_type)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: &StatementType) -> Result<()> {
        match &statement {
            StatementType::Assign(_) => todo!(),
            StatementType::Return(_) => todo!(),
            StatementType::Expression(expression) => {
                self.compile_expression(expression)?;
                self.add_instruction(Instruction::Pop);
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<()> {
        match expression {
            Expression::Infix {
                left,
                right,
                operator,
            } => self.compile_infix_expersion(left.as_ref(), right.as_ref(), operator)?,
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
                self.compile_expression(right)?;

                match operator {
                    PrefixOperator::Bang => {
                        self.add_instruction(Instruction::Bang);
                    }
                    PrefixOperator::Minus => {
                        self.add_instruction(Instruction::Minus);
                    }
                }
            }
            Expression::If(if_expression) => self.compile_if_condition(&if_expression)?,
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::IdentifierLiteral(_) => todo!(),
        }

        Ok(())
    }

    fn compile_infix_expersion(
        &mut self,
        left: &Expression,
        right: &Expression,
        operator: &Operator,
    ) -> Result<()> {
        match operator {
            Operator::Bang => todo!(),
            Operator::Minus => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::Sub)
            }
            Operator::Plus => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::Add)
            }
            Operator::Multiply => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::Mul)
            }
            Operator::Equals => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::Equal)
            }
            Operator::NotEquals => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::NotEqual)
            }
            Operator::GreaterThan => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::GreaterThan)
            }
            Operator::LessThan => {
                self.compile_expression(right)?;
                self.compile_expression(left)?;
                self.add_instruction(Instruction::GreaterThan)
            }
            Operator::DividedBy => {
                self.compile_left_right(left, right)?;
                self.add_instruction(Instruction::Div)
            }
        };

        Ok(())
    }

    fn compile_left_right(&mut self, left: &Expression, right: &Expression) -> Result<()> {
        self.compile_expression(left)?;
        self.compile_expression(right)
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

        self.previous_instruction = self.last_instruction.take();
        self.last_instruction = Some(EmittedInstruction {
            instruction,
            position: new_instruction_position,
        });

        new_instruction_position
    }

    fn compile_if_condition(&mut self, if_expression: &&if_expression::IfExpression) -> Result<()> {
        self.compile_expression(&if_expression.condition)?;

        // TODO: This will be replaced somewhere else in the code, consider using an optional
        let jump_not_truthy_position = self.add_instruction(Instruction::JumpNotTruthy(9999));

        self.compile_blockstatement(&if_expression.consequence)?;

        if self.if_last_instruction_is_pop() {
            self.remove_last_pop();
        }

        match &if_expression.alternative {
            None => {
                let after_consequence_position = self.instructions.len();
                let after_consequence_position: u16 = u16::try_from(after_consequence_position)?;
                self.replace_instructions(
                    jump_not_truthy_position,
                    Instruction::JumpNotTruthy(after_consequence_position),
                );
            }
            Some(alternative) => {
                let jump_truthy_pos = self.add_instruction(Instruction::Jump(9999));

                let after_consequence_position = self.instructions.len();
                let after_consequence_position: u16 = u16::try_from(after_consequence_position)?;
                self.replace_instructions(
                    jump_not_truthy_position,
                    Instruction::JumpNotTruthy(after_consequence_position),
                );

                self.compile_blockstatement(alternative)?;

                if self.if_last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                let after_alternative_position = self.instructions.len();
                let after_alternative_position: u16 = u16::try_from(after_alternative_position)?;
                self.replace_instructions(
                    jump_truthy_pos,
                    Instruction::Jump(after_alternative_position),
                );
            }
        }

        Ok(())
    }

    fn compile_blockstatement(&mut self, consequence: &BlockStatement) -> Result<()> {
        for statement in &consequence.statements {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn replace_instructions(&mut self, operation_position: usize, instruction: Instruction) {
        self.instructions[operation_position] = instruction;
    }

    fn if_last_instruction_is_pop(&self) -> bool {
        if let Some(EmittedInstruction {
            instruction: Instruction::Pop,
            ..
        }) = self.last_instruction
        {
            return true;
        }
        false
    }

    fn remove_last_pop(&mut self) {
        self.instructions.pop();
        self.last_instruction = self.previous_instruction.take();
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

    #[test]
    fn test_conditional_expressions() {
        let test_cases: Vec<CompilerTestCase> = vec![
            CompilerTestCase {
                input: String::from("if (true):  10~ 3333"),
                expected_constants: vec![
                    PrimitiveObject::Integer(10),
                    PrimitiveObject::Integer(3333),
                ],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::JumpNotTruthy(3),
                    Instruction::OpConstant(0),
                    Instruction::Pop,
                    Instruction::OpConstant(1),
                    Instruction::Pop,
                ],
            },
            CompilerTestCase {
                input: String::from("if true: 10 else: 20~ 3333"),
                expected_constants: vec![
                    PrimitiveObject::Integer(10),
                    PrimitiveObject::Integer(20),
                    PrimitiveObject::Integer(3333),
                ],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::JumpNotTruthy(4),
                    Instruction::OpConstant(0),
                    Instruction::Jump(5),
                    Instruction::OpConstant(1),
                    Instruction::Pop,
                    Instruction::OpConstant(2),
                    Instruction::Pop,
                ],
            },
        ];

        test_util::run_compiler_tests(test_cases);
    }
}
