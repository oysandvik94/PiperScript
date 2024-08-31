pub mod runtime_error;

use crate::{
    compiler::{bytecode::Instruction, internal_error::InternalError, ByteCode},
    eval::objects::{Object, PrimitiveObject},
};

use anyhow::{bail, Result};
use runtime_error::RuntimeError;

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    constants: Vec<PrimitiveObject>,
    instructions: Vec<Instruction>,
    stack: Vec<Object>,
    last_popped: Object,
}

impl VirtualMachine {
    pub fn new(bytecode: ByteCode) -> VirtualMachine {
        VirtualMachine {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            last_popped: Object::Void,
        }
    }

    pub fn run(&mut self) -> Result<Object> {
        let mut instruction_pointer = 0;
        while instruction_pointer < self.instructions.len() {
            // To avoid borrowing conflicts, retrieve the operation index first
            let operation = &self.instructions[instruction_pointer];
            instruction_pointer += 1;

            match operation {
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
            };
        }

        Ok(self.last_popped.clone())
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
}
