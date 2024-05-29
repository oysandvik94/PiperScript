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
foo: 5

if foo == 5:
    foo + 10
else:
    1 + 1
~

add: fn(x, y): return x + y~

add 5 1

complex: fn(x, y, k):
    foo: add x y
    return k(foo)
~

power: fn(x): return x * x~

complex 5 3 power // 64
res: complex 5 3 power
```

There is nothing interesting or worthwhile about the language, the project is
entirely about the exercise of creating an interpreter.
