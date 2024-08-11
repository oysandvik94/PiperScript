let foo: "hei"
let bar: fn(x):
    return x + "ok"
~

if bar("en") == foo:
    print(bar)
else:
    print(foo)
~

let adder: fn(x):
    return fn(y): return x + y ~
~

let array: [2, 5, 6]
print(array[2])

let hash: {"mee": 5, "boo": 1}
print(hash["mee"])

let one: 1
print(hash["mee"] / 8 * (array[1] + one))

print(last(array))

push(array, 5)

