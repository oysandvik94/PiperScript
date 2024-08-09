# PiperScript

This project is an interpreter for the language PiperScript.

It is intented as a learning exercise based on the book [Writing An Interpreter
in Go](https://interpreterbook.com/). At the current state the intepreter is
adapted from Go to Rust, with the goal of learning more about interpreters
and Rust.

PiperScript looks something like this:

```
let foo: "hei"
let bar: fn(x):
    return x + "ok"
~

if bar == foo:
    print(bar)
else:
    print(foo)
~

let adder: fn(x):
    return fn(y): return x + y ~
~

let array: [2, 5, 6]
print(array[2])

let hash: {"mee", 5, "boo", 1}
print(hash["mee"])

let one: 1
print(hash["mee"] / 8 * (array[1] + one))

print(last(array))

push(array, 5)
```

## Additonal documentation

See the following readme's for more technical docs:

- [Interpreter](crates/interpreter/README.md)
- [LSP](crates/son_of_anton/README.md)
- [Formatter](crates/piperfmt/README.md)
