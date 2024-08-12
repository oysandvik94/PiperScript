pub mod runtime_error;

use crate::{
    compiler::{
        bytecode::{self, Instructions, OpCode},
        ByteCode,
    },
    eval::objects::{Object, PrimitiveObject},
};

use anyhow::{bail, Result};
use runtime_error::RuntimeError;

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    constants: Vec<PrimitiveObject>,
    instructions: Instructions,
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
        while instruction_pointer < self.instructions.0.len() {
            let operation_byte: u8 = self.instructions.0[instruction_pointer];
            let operation = bytecode::OpCode::try_from(operation_byte)?;

            match operation {
                OpCode::OpConstant => {
                    instruction_pointer += 1;
                    let operands: [u8; 2] = [
                        self.instructions.0[instruction_pointer],
                        self.instructions.0[instruction_pointer + 1],
                    ];

                    let constant_index = self.read_u16(operands);
                    instruction_pointer += 2;

                    let constant = self.constants[constant_index].clone();
                    self.push(Object::Primitive(constant))?;
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

    fn read_u16(&self, bytes: [u8; 2]) -> usize {
        u16::from_be_bytes(bytes) as usize
    }

    // fn read_u16(&self, instruction_pointer: usize) -> usize {
    //     let bytes = &self.instructions.0[instruction_pointer..instruction_pointer + 2];
    //     u16::from_be_bytes([bytes[0], bytes[1]]) as usize
    // }
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
                expected: 2,
            },
        ];

        test_util::run_vm_tests(tests);
    }
}
