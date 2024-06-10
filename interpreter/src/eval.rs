use evaluator::Evaluable;
use objects::Object;

use crate::parser::{lexer::lexedtokens::LexedTokens, Parser};

mod eval_error;
mod evaluator;
mod objects;

pub fn eval(input: &str) -> Result<Object, eval_error::EvalError> {
    let lexed_tokens = LexedTokens::from(input);
    let program = Parser::parse_tokens(lexed_tokens);

    program.eval()
}
