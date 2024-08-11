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
else if b > 40:
    print("Large number")
else:
    print("Something else")
~

let i: 0
while i < 5:
    print(numbers[i])
    let i: i + 1
~

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
else if x < 20:
    if x % 2 == 0:
        print("Medium even")
    else:
        print("Medium odd")
    ~
else:
    print("Large")
~

let create_multiplier: fn(factor):
    return fn(x): return x * factor ~
~

let double: create_multiplier(2)
print(double(5))

let factorial: fn(n):
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)
    ~
~

print(factorial(5))
