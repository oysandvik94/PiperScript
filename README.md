# Lasagnalang

This project is an interpreter for the language Lasagnalang.

It is intented as a learning exercise based on the book [Writing An Interpreter
in Go](https://interpreterbook.com/). At the current state the intepreter is
adapted from the Go to Rust, with the goal of learning more about interpreters
and Rust. At a later stage I hope to create a
[LSP](https://microsoft.github.io/language-server-protocol/) for Lasagnalang,
as well as implementing some more novel idea for the language.

Lasagnalang may look something like this:

```
~foo: 5~

~if foo is 5~
    foo + 10
~

~bar: fn(x, y): return x + y~

bar(5, 1)
```

There is nothing interesting or worthwhile about the language, the project is
entirely about the exercise of creating an interpreter.
