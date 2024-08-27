let a: "hello world"
let b: 42
let c: true
let d: false
let e: null

let add: fn(x, y):
    return x + y
~

let multiply: fn(x, y):
    return x * y
~

let complex_func: fn(x, y, z):
    let temp: x + y
    if temp > z:
        return temp * 2
    else:
        return z - temp
    ~
~

let outer: fn(x):
    let inner: fn(y):
        return x * y
    ~
    return inner
~

let numbers: [1, 2, 3, 4, 5]
let mixed_array: [1, "two", true, [4, 5], {"six": 6}]
let person: {"name": "Alice", "age": 30, "hobbies": ["reading", "coding"]}

if a == "hello world":
    print("Greeting")
else:
    print("Something else")
~

let i: 0

print(add(b, 10))
print(multiply(3, 4))
print(complex_func(5, 7, 10))

print(outer(5)(3))

push(numbers, 6)
print(last(numbers))
print(person["name"])
print(mixed_array[3][1])

let result: (b / 2 * (numbers[2] + 1)) - person["age"]
print(result)

let concat_result: a + " " + person["name"]
print(concat_result)

let nested: {
    "data": [
        {"id": 1, "value": "first"},
        {"id": 2, "value": "second"},
        {"id": 3, "value": "third"}
    ],
    "metadata": {
        "created": "2024-08-11",
        "author": "Parser Benchmark"
    }
}

print(nested["data"][1]["value"])
print(nested["metadata"]["author"])

let x: 15
if x < 10:
    print("Small")
else: 
    if x + 2 == 0:
        print("Medium even")
    else:
        print("Medium odd")
    ~
~

let create_multiplier: fn(factor):
    return fn(x): return x * factor ~
~

let double: create_multiplier(2)
print(double(5))

let factorial: fn(n):
    if n < 1:
        return 1
    else:
        return n * factorial(n - 1)
    ~
~

print(factorial(5))

let vara: 1
let varb: 2
let varc: 3
let vard: 4

let deeply_nested_func: fn(xa):
    let funcb: fn(xb):
        let funcc: fn(xc):
            let funcd: fn(xd):
                return xa + xb + xc + xd
            ~
        ~
    ~
~

let large_nested_structure: {
    "levela": {
        "levelb": {
            "levelc": {
                "leveld": {
                    "data": [1, 2, 3, 4]
                }
            }
        }
    }
}

let complex_function_recursive: fn(pa, pb, pc, pd, i, result):
    if i > 1000000:
        return result
    else:
        let new_result: if i / 2 == 0:
            result + (pa * i) / pb
        else:
            result - (pc * i) / pd
        ~
        complex_function_recursive(pa, pb, pc, pd, i + 1, new_result)
    ~
~

let complex_function: fn(pa, pb, pc, pd):
    complex_function_recursive(pa, pb, pc, pd, 1, 0)
~

print(complex_function(1, 2, 3, 4))
print(complex_function(5, 6, 7, 8))

let massive_expression: (vara + varb) * (varc - vard) / (vara * varb) + 
                        (varb + varc) * (vard - vara) / (varb * varc) +
                        (varc + vard) * (vara - varb) / (varc * vard)

print(massive_expression)

let nested_condition: fn(x, depth):
    if depth == 0:
        print("Deeply nested condition reached")
    else:
        if x == 1:
            if x < 2:
                if x > 0:
                    nested_condition(x, depth - 1)
                ~
            ~
        ~
    ~
~

nested_condition(1, 10)

let deep_recursion: fn(n):
    if n == 0:
        0
    else:
        1 + deep_recursion(n - 1)
    ~
~

print(deep_recursion(100))

let generate_sequence: fn(start, end):
    if start > end:
        []
    else:
        [start] + generate_sequence(start + 1, end)
    ~
~

print(generate_sequence(1, 10))
