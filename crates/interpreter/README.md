# Interpreter

The interpreter is currently a tree-walking interpreter.

## Parsing

The main entry point is in `parser.rs`, see `Parser` structs new function.

The parser holds a lexer, that walks the source code and produces
tokens for the parser.

The parser is a [Pratt Parser](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html), also known as
recursive descent parsing.

The parser produces human readable "compile-time" errors.

## Evaluating

Currently a tree-walking interpreter. Will be rewritten to
byte-code interpreter soon.

Main entry point is in `eval.rs`, see `eval::eval()`

Evaluator produces runtime errors.
