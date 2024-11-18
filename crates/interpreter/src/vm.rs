pub mod runtime_error;

use crate::{
    compiler::{bytecode::Instruction, internal_error::InternalError, ByteCode},
    eval::objects::{Object, PrimitiveObject},
};

use anyhow::{bail, Ok, Result};
use runtime_error::RuntimeError;

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    constants: Vec<PrimitiveObject>,
    instructions: Vec<Instruction>,
    instruction_pointer: usize,
    stack: Vec<Object>,
    last_popped: Object,
}

impl VirtualMachine {
    pub fn new(bytecode: ByteCode) -> VirtualMachine {
        VirtualMachine {
            instructions: bytecode.instructions,
            instruction_pointer: 0,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            last_popped: Object::Void,
        }
    }

    pub fn run(&mut self) -> Result<Object> {
        // let mut instruction_pointer = 0;
        while self.instruction_pointer < self.instructions.len() {
            // To avoid borrowing conflicts, retrieve the operation index first
            // let operation = &self.instructions[instruction_pointer];

            match &self.instructions[self.instruction_pointer] {
                Instruction::OpConstant(constant_index) => {
                    // TODO: We can probably avoid cloning here if we store references on the stack?
                    let constant = self.constants[*constant_index as usize].clone();
                    self.push(Object::Primitive(constant))?;
                }
                Instruction::Pop => {
                    // Call `pop` on `self`, which is a mutable borrow
                    self.last_popped = self.pop()?;
                }
                Instruction::Sub => {
                    let (left, right) = self.execute_integer_binary()?;
                    let res = left - right;

                    self.push(Object::primitive_from_int(res))?;
                }
                Instruction::Add => {
                    let (left, right) = self.execute_integer_binary()?;
                    let res = left + right;

                    self.push(Object::primitive_from_int(res))?;
                }
                Instruction::Mul => {
                    let (left, right) = self.execute_integer_binary()?;
                    let res = left * right;

                    self.push(Object::primitive_from_int(res))?;
                }
                Instruction::Div => {
                    let (left, right) = self.execute_integer_binary()?;
                    let res = left / right;

                    self.push(Object::primitive_from_int(res))?;
                }
                // TODO: Check if it matters to use constants here for boolean
                Instruction::True => self.push(Object::primitive_from_bool(true))?,
                Instruction::False => self.push(Object::primitive_from_bool(false))?,
                Instruction::Equal | Instruction::NotEqual | Instruction::GreaterThan => {
                    self.execute_comparison()?;
                }
            };

            self.instruction_pointer += 1;
        }

        Ok(self.last_popped.clone())
    }

    fn execute_comparison(&mut self) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        let operation = &self.instructions[self.instruction_pointer];
        match (left, right) {
            (
                Object::Primitive(PrimitiveObject::Integer(left)),
                Object::Primitive(PrimitiveObject::Integer(right)),
            ) => self.execute_integer_comparison(&left, &right),
            (
                Object::Primitive(PrimitiveObject::Boolean(left)),
                Object::Primitive(PrimitiveObject::Boolean(right)),
            ) => match operation {
                Instruction::Equal => self.push(Object::primitive_from_bool(left == right)),
                Instruction::NotEqual => self.push(Object::primitive_from_bool(left != right)),
                _ => Ok(()),
            },
            _ => bail!(RuntimeError::TypeError),
        }
    }

    fn execute_integer_comparison(&mut self, left: &i32, right: &i32) -> Result<()> {
        match self.instructions[self.instruction_pointer] {
            Instruction::Equal => self.push(Object::primitive_from_bool(left == right)),
            Instruction::NotEqual => self.push(Object::primitive_from_bool(left != right)),
            Instruction::GreaterThan => self.push(Object::primitive_from_bool(left > right)),
            _ => Ok(()),
        }
    }

    /// Unclear whether we need to manage a stack stack_pointer
    /// at this point in the book, or if we can just use the stack api like normal
    fn push(&mut self, object: Object) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            bail!(RuntimeError::StackOverflowError);
        }

        // self.stack[self.stack_pointer] = object;
        // self.stack_pointer += 1;

        self.stack.push(object);

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        match self.stack.pop() {
            Some(object) => Ok(object),
            None => bail!(InternalError::PoppedEmptyStack),
        }
    }

    fn execute_integer_binary(&mut self) -> Result<(i32, i32)> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (left, right) {
            (
                Object::Primitive(PrimitiveObject::Integer(left)),
                Object::Primitive(PrimitiveObject::Integer(right)),
            ) => Ok((left, right)),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_util::{self, VmTestCase};

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VmTestCase<i32>> = vec![
            VmTestCase {
                input: "1",
                expected: 1,
            },
            VmTestCase {
                input: "2",
                expected: 2,
            },
            VmTestCase {
                input: "1 + 2",
                expected: 3,
            },
            VmTestCase {
                input: "5 - 3",
                expected: 2,
            },
            VmTestCase {
                input: "4 * 3",
                expected: 12,
            },
            VmTestCase {
                input: "10 / 2",
                expected: 5,
            },
        ];

        test_util::run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expression() {
        let tests: Vec<VmTestCase<bool>> = vec![
            VmTestCase {
                input: "false",
                expected: false,
            },
            VmTestCase {
                input: "true",
                expected: true,
            },
            VmTestCase {
                input: "1 < 2",
                expected: true,
            },
            VmTestCase {
                input: "1 > 2",
                expected: false,
            },
            VmTestCase {
                input: "1 < 1",
                expected: false,
            },
            VmTestCase {
                input: "1 > 1",
                expected: false,
            },
            VmTestCase {
                input: "1 == 1",
                expected: true,
            },
            VmTestCase {
                input: "1 != 1",
                expected: false,
            },
            VmTestCase {
                input: "1 == 2",
                expected: false,
            },
            VmTestCase {
                input: "1 != 2",
                expected: true,
            },
            VmTestCase {
                input: "true == true",
                expected: true,
            },
            VmTestCase {
                input: "false == false",
                expected: true,
            },
            VmTestCase {
                input: "true != false",
                expected: true,
            },
            VmTestCase {
                input: "false != true",
                expected: true,
            },
            VmTestCase {
                input: "(1 < 2) == true",
                expected: true,
            },
            VmTestCase {
                input: "(1 < 2) == false",
                expected: false,
            },
            VmTestCase {
                input: "(1 > 2) == true",
                expected: false,
            },
            VmTestCase {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        test_util::run_vm_tests(tests);
    }
}
