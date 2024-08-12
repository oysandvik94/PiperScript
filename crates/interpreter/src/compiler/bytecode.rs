use std::fmt::Display;

use anyhow::Result;

use crate::compiler::bytecode;

use super::internal_error::InternalError;

/// Not sure if we need this wrapper, or if we could
/// just use a type alias.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Instructions(pub Vec<u8>);

#[repr(u8)]
#[derive(Clone)]
pub enum OpCode {
    OpConstant = 0,
    Add = 1,
}

impl TryFrom<u8> for OpCode {
    type Error = InternalError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::OpConstant),
            1 => Ok(OpCode::Add),
            unknown_opcode => Err(InternalError::UnknownOpcode(unknown_opcode)),
        }
    }
}

impl OpCode {
    pub fn operand_widths(&self) -> Vec<i32> {
        match self {
            OpCode::OpConstant => vec![2],
            OpCode::Add => vec![],
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpConstant => write!(f, "OpConstant"),
            OpCode::Add => write!(f, "Add"),
        }
    }
}

/// This method closely resembles the code in the book
/// Will be interesting to see once VM can do fibonacci if we
/// can make this more rusty and still have good performance
pub fn make(op_code: OpCode, operands: &[u16]) -> Vec<u8> {
    let mut instruction_length = 1;
    let op_width = op_code.operand_widths();

    for width in &op_width {
        instruction_length += width;
    }

    let mut instruction: Vec<u8> = Vec::with_capacity(instruction_length as usize);

    instruction.push(op_code as u8);

    for (idx, operand) in operands.iter().enumerate() {
        let width = op_width[idx];
        match width {
            1 => instruction.push(*operand as u8),
            2 => instruction.extend_from_slice(&operand.to_be_bytes()),
            _ => panic!("Unsupported operand width: {}", width),
        }
    }

    instruction
}

fn read_operands(op: &OpCode, instruction: &[u8]) -> (Vec<u16>, u32) {
    let operand_widths = op.operand_widths();
    let mut operands = Vec::with_capacity(operand_widths.len());
    let mut offset = 0;
    for width in operand_widths {
        match width {
            1 => {
                operands.push(instruction[offset] as u16);
                offset += 1;
            }
            2 => {
                let operand = u16::from_be_bytes([instruction[offset], instruction[offset + 1]]);
                operands.push(operand);
                offset += 2;
            }
            _ => panic!("Unsupported operand width: {}", width),
        }
    }

    (operands, offset as u32)
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut n = 0;
        while n < self.0.len() {
            let op_code = bytecode::OpCode::try_from(self.0[n]).map_err(|_| std::fmt::Error)?;
            let (operands, read) = bytecode::read_operands(&op_code, &self.0[n + 1..]);

            if op_code.operand_widths().len() != operands.len() {
                write!(f, "ERROR: Operand len does not match defined")?;
                return Ok(());
            }

            let operand_str = match operands.len() {
                1 => format!("{op_code} {}", operands[0]),
                count => format!("{op_code} Unhandled operator count for {count}"),
            };

            writeln!(f, "{:04} {}", n, operand_str)?;

            n += 1 + read as usize
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::bytecode;

    #[test]
    fn test_read_operands() {
        let tests = vec![(OpCode::OpConstant, vec![65535], 2)];

        for test in tests {
            let instruction = bytecode::make(test.0.clone(), &test.1);

            let (read_operands, bytes_read) = bytecode::read_operands(&test.0, &instruction[1..]);
            assert_eq!(test.2, bytes_read);

            read_operands
                .into_iter()
                .zip(test.1)
                .for_each(|(actual, expected)| {
                    assert_eq!(expected, actual, "Should decompile");
                });
        }
    }

    #[test]
    #[ignore = "The decompiler is a total mess"]
    fn test_instruction_strings() {
        let instructions = [
            bytecode::make(OpCode::Add, &[]),
            bytecode::make(OpCode::OpConstant, &[2]),
            bytecode::make(OpCode::OpConstant, &[65535]),
        ];

        let expected = "0000 OpAdd\n\
            0003 OpConstant 2\n\
            0006 OpConstant 65535\n";

        let instructions = Instructions(instructions.concat());

        assert_eq!(expected, format!("{instructions}"));
    }
    #[test]
    fn test_make() {
        struct TestCase {
            op: OpCode,
            operands: Vec<u16>,
            expected: Vec<u8>,
        }

        let test_cases: Vec<TestCase> = vec![
            TestCase {
                op: OpCode::OpConstant,
                operands: vec![65534],
                expected: vec![OpCode::OpConstant as u8, 255, 254],
            },
            TestCase {
                op: OpCode::Add,
                operands: vec![],
                expected: vec![OpCode::Add as u8],
            },
        ];

        for test_case in test_cases {
            let instruction = make(test_case.op, &test_case.operands).to_vec();
            let instruction = Instructions(instruction);
            let expected_instructions = Instructions(test_case.expected);

            assert_eq!(
                instruction, expected_instructions,
                "instruction has wrong byte encoding. want={}, got={}",
                expected_instructions, instruction
            );
        }
    }
}
