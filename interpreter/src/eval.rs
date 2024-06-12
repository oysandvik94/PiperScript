use eval_error::EvalError;
use evaluator::Evaluable;
use objects::Object;

use crate::parser::{
    lexer::lexedtokens::LexedTokens, parse_errors::ParseError, ParsedProgram, Parser,
};

mod eval_error;
mod evaluator;
pub mod objects;

pub enum EvaledProgram {
    ParseError(Vec<ParseError>),
    EvalError(EvalError),
    Valid(Object),
}

pub fn eval(input: &str) -> EvaledProgram {
    let lexed_tokens = LexedTokens::from(input);
    let program = Parser::parse_tokens(lexed_tokens);

    match program {
        ParsedProgram::InvalidProgram(parse_errors) => EvaledProgram::ParseError(parse_errors),
        ParsedProgram::ValidProgram(valid_program) => {
            let mut object: Option<Object> = None;

            for statement in valid_program {
                object = match statement.eval() {
                    Ok(evaled_object) => Some(evaled_object),
                    Err(eval_error) => return EvaledProgram::EvalError(eval_error),
                }
            }

            match object {
                Some(object) => EvaledProgram::Valid(object),
                None => EvaledProgram::EvalError(EvalError::EmptyProgram),
            }
        }
    }
}
