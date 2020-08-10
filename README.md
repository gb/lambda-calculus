# Lambda Calculus
An extremely simple implementation of Lambda-Calculus Interpreter.


## Motivation 
During my Compilers and Programming languages class (CS-421 @ UIUC), [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) was one of the topics that really caught my eyes,
for explain how function application works and what functions can do, and actually how functional languages are build
around it.

This project is my attempt of implement a simple lambda calculus expression parser.

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade) 

## Setup

```
git clone git@github.com:gb/lambda-calculus.git
cd lambda-calculus
make run
```

## Example
```
Please type a lambda expression:
λ> (\x.x) a
((λx.x) a)
a
λ> (\x.x x) a
((λx.(x x)) a)
(a a)
λ> (\x.y x) a
((λx.(y x)) a)
(y a)
λ> (\x.\a.x) a
((λx.(λa.x)) a)
(λb.a)
λ> (\x.\x.x) a
((λx.(λx.x)) a)
(λx.x)
λ> (\x.(\y.y) x) a
((λx.((λy.y) x)) a)
((λy.y) a)
a
λ> (\x.\a.x) a
((λx.(λa.x)) a)
(λb.a)
```

## Tests

A good way to understand the code intention is checking the [test suite](test/Test.hs). To execute the tests:
```
make test
```