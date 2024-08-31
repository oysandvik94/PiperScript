pub mod runtime_error;

use crate::{
    compiler::{bytecode::OpCode, internal_error::InternalError, ByteCode},
    eval::objects::{Object, PrimitiveObject},
};

use anyhow::{bail, Result};
use runtime_error::RuntimeError;

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    constants: Vec<PrimitiveObject>,
    instructions: Vec<OpCode>,
    stack: Vec<Object>,
    stack_pointer: usize,
}

impl VirtualMachine {
    pub fn new(bytecode: ByteCode) -> VirtualMachine {
        VirtualMachine {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,
        }
    }

    pub fn run(&mut self) -> Result<Object> {
        let mut instruction_pointer = 0;
        while instruction_pointer < self.instructions.len() {
            let operation: &OpCode = &self.instructions[instruction_pointer];
            instruction_pointer += 1;

            match operation {
                OpCode::OpConstant(constant_index) => {
                    let constant = self.constants[*constant_index as usize].clone();
                    self.push(Object::Primitive(constant))?;
                }
                OpCode::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;

                    let res = match (right, left) {
                        (
                            Object::Primitive(PrimitiveObject::Integer(left)),
                            Object::Primitive(PrimitiveObject::Integer(right)),
                        ) => left + right,
                        _ => todo!(),
                    };

                    self.push(Object::primitive_from_int(res))?;
                }
            }
        }

        Ok(Object::Void)
    }

    /// Unclear whether we need to manage a stack stack_pointer
    /// at this point in the book, or if we can just use the stack api like normal
    fn push(&mut self, object: Object) -> Result<()> {
        if self.stack_pointer >= STACK_SIZE {
            bail!(RuntimeError::StackOverflowError);
        }

        // self.stack[self.stack_pointer] = object;
        // self.stack_pointer += 1;

        self.stack.push(object);
        self.stack_pointer += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        self.stack_pointer -= 1;
        match self.stack.pop() {
            Some(object) => Ok(object),
            None => bail!(InternalError::PoppedEmptyStack),
        }
    }

    pub fn stack_top(&self) -> Object {
        if self.stack_pointer == 0 {
            return Object::Void;
        }

        // This can panic, so should be handled,
        // but check later if we can use safe api
        // without losing performance instead
        // self.stack[self.stack_pointer].clone()

        self.stack.last().cloned().unwrap_or(Object::Void)
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
        ];

        test_util::run_vm_tests(tests);
    }
}
