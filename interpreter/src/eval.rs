use eval_error::EvalError;
use objects::{EnvReference, Object};

use crate::parser::{
    lexer::lexedtokens::LexedTokens, parse_errors::ParseError, ParsedProgram, Parser,
};

mod eval_error;
mod expression_evaluator;
pub mod function_evaluator;
pub mod objects;
mod statement_evaluator;

pub enum EvaledProgram {
    ParseError(Vec<ParseError>),
    EvalError(EvalError),
    Valid(Object),
}

pub fn eval(input: &str, env: &mut EnvReference) -> EvaledProgram {
    let lexed_tokens = LexedTokens::from(input);
    let program = Parser::parse_tokens(lexed_tokens);

    match program {
        ParsedProgram::InvalidProgram(parse_errors) => EvaledProgram::ParseError(parse_errors),
        ParsedProgram::ValidProgram(valid_program) => {
            let evaled = statement_evaluator::eval_statements(&valid_program, env);

            match evaled {
                Ok(evaled) => EvaledProgram::Valid(evaled),
                Err(errored) => EvaledProgram::EvalError(errored),
            }
        }
    }
}
