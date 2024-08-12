use std::{fmt::Display, sync::Once};

use tracing::{event, Level};
use tracing_subscriber::FmtSubscriber;

use crate::{
    compiler::{bytecode::Instructions, Compiler},
    eval::{
        self,
        objects::{Environment, Object, PrimitiveObject},
        EvaledProgram,
    },
    parser::{
        ast::{BlockStatement, Identifier, Operator, PrefixOperator, Statement, StatementType},
        expressions::{
            expression::Expression, functions::FunctionLiteral, if_expression::IfExpression,
        },
        lexer::{token::Token, Lexer},
        ParsedProgram, Parser,
    },
    vm::VirtualMachine,
};

pub fn assert_list<T, K, F>(test_cases: Vec<(T, K)>, mut asserter: F)
where
    F: FnMut(&K, &T),
    K: PartialEq + std::fmt::Debug,
{
    test_cases.iter().for_each(|(input, expected)| {
        asserter(expected, input);
    });
}

pub fn assert_integar_literal(actual: &Object, expected: i32) {
    match actual {
        Object::Primitive(actual) => {
            match actual {
                PrimitiveObject::Integer(actual) => {
                    assert_eq!(expected, *actual, "Expected {expected} but got {actual}")
                }
                unexpected => panic!("Expected integer literal, but got {unexpected}"),
            }
            {}
        }
        unexpected => panic!("Expected integer literal, but got {unexpected}"),
    }
}

pub fn has_parser_errors(program: &ParsedProgram) -> bool {
    match program {
        ParsedProgram::ValidProgram(_) => false,
        ParsedProgram::InvalidProgram(parse_errors) => {
            eprintln!("Found parser errors:");
            for parse_error in parse_errors {
                eprintln!("parser error: {parse_error}");
            }

            true
        }
    }
}

pub fn parse_program(source_code: &str) -> ParsedProgram {
    let mut parser = Parser::new(source_code);
    parser.parse_program()
}

pub fn expect_evaled_program(source_code: &str) -> Object {
    match eval::eval(source_code, &mut Environment::new_env_reference()) {
        EvaledProgram::ParseError(parse_errors) => {
            parse_errors.into_iter().for_each(|ele| {
                eprintln!("{ele}");
            });
            panic!("Eval failed with parse errors");
        }
        EvaledProgram::EvalError(eval_errors) => {
            eprintln!("{eval_errors}");
            panic!("Eval failed with runtime errors")
        }
        EvaledProgram::Valid(valid_program) => valid_program,
    }
}

pub fn expect_parsed_program(source_code: &str) -> Vec<Statement> {
    let mut parser = Parser::new(source_code);
    let evaled_program = parser.parse_program();
    match evaled_program {
        ParsedProgram::ValidProgram(valid_statements) => valid_statements,
        ParsedProgram::InvalidProgram(parse_errors) => {
            parse_errors.into_iter().for_each(|ele| eprintln!("{ele}"));
            panic!("parse failed with parse errors")
        }
    }
}

pub fn tokenize(source_code: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source_code);
    let mut tokens: Vec<Token> = Vec::new();

    event!(Level::DEBUG, "hei");
    while let Some(token) = lexer.consume() {
        event!(Level::DEBUG, "pushing token: {token:?}");
        tokens.push(token);
    }

    tokens
}

pub fn create_prefix_test_case(
    right_expression: Expression,
    operator: PrefixOperator,
) -> StatementType {
    StatementType::Expression(Expression::Prefix {
        right: Box::new(right_expression),
        operator,
    })
}

pub fn create_infix_test_case(
    left_expression: Expression,
    right_expression: Expression,
    operator: Operator,
) -> StatementType {
    StatementType::Expression(Expression::Infix {
        left: Box::new(left_expression),
        right: Box::new(right_expression),
        operator,
    })
}

pub fn create_function_expression(parameters: Vec<&str>, body: BlockStatement) -> StatementType {
    StatementType::Expression(Expression::Function(FunctionLiteral {
        parameters: parameters
            .iter()
            .map(|param| Identifier(param.to_string()))
            .collect(),
        body,
    }))
}

pub fn create_if_condition(
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
) -> StatementType {
    use Expression::*;
    StatementType::Expression(If(IfExpression {
        condition: Box::from(condition),
        consequence,
        alternative,
    }))
}

pub fn create_integer_infix_expression(left: i32, right: i32, operator: Operator) -> Expression {
    Expression::Infix {
        left: Box::from(Expression::IntegerLiteral(left)),
        right: Box::from(Expression::IntegerLiteral(right)),
        operator,
    }
}

pub fn create_infix_expression(
    left: Expression,
    right_expression: Expression,
    operator: Operator,
) -> Expression {
    Expression::Infix {
        right: Box::from(right_expression),
        left: Box::from(left),
        operator,
    }
}

pub fn create_identifierliteral(literal: &str) -> Expression {
    Expression::IdentifierLiteral(Identifier(literal.to_string()))
}

static TRACING: Once = Once::new();
pub fn setup_logger() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    TRACING.call_once(|| {
        tracing::subscriber::set_global_default(subscriber)
            .expect("setting default subscriber failed");
    })
}

pub struct CompilerTestCase {
    pub input: String,
    pub expected_constants: Vec<PrimitiveObject>,
    pub expected_instructions: Vec<Instructions>,
}

pub fn run_compiler_tests(test_cases: Vec<CompilerTestCase>) {
    for test_case in test_cases {
        let ast = expect_parsed_program(&test_case.input);

        let mut compiler: Compiler = Compiler::default();

        compiler.compile(ast);

        let bytecode = compiler.bytecode();

        test_instructions(test_case.expected_instructions, bytecode.instructions);
        test_constants(test_case.expected_constants, bytecode.constants);
    }
}

fn test_instructions(expected_instructions: Vec<Instructions>, instructions: Instructions) {
    let expected_instructions: Vec<u8> = expected_instructions
        .into_iter()
        .flat_map(|instr| instr.0)
        .collect();

    assert_eq!(
        expected_instructions.len(),
        instructions.0.len(),
        "Instuctions should have same size"
    );

    instructions
        .0
        .into_iter()
        .zip(expected_instructions)
        .for_each(|(actual, expected)| assert_eq!(expected, actual, "Wrong instruction."));
}

fn test_constants(expected_constants: Vec<PrimitiveObject>, constants: Vec<PrimitiveObject>) {
    assert_eq!(expected_constants.len(), constants.len());

    expected_constants
        .into_iter()
        .zip(constants)
        .for_each(|(expected, actual)| assert_eq!(expected, actual, "Got incorrect constant"));
}

pub struct VmTestCase<T> {
    pub input: &'static str,
    pub expected: T,
}

pub fn run_vm_tests<T: Display>(test_cases: Vec<VmTestCase<T>>) {
    for test_case in test_cases {
        let ast = expect_parsed_program(test_case.input);

        let mut compiler: Compiler = Compiler::default();
        compiler.compile(ast);
        let bytecode = compiler.bytecode();

        let mut vm = VirtualMachine::new(bytecode);
        vm.run().unwrap();

        let stack_elem = vm.stack_top();

        assert_eq!(test_case.expected.to_string(), stack_elem.to_string());
    }
}
