# My programming language
This is an experimental programming language used for learning, written in OCaml `4.06.1`.

## Current features
* Simple arithmetics (`+`, `-`, `*`, `/`, `>`, `<`) with precences
* `int` and `bool` types
* `let` bindings and `if` expressions
* Closures `fun x: int -> x*x`
* Strict, static typechecker
* Partial evaluation:
* Multiple argument functions
```
$ dune build repl.exe && dune exec ./repl.exe
> let a = ((fun x:int -> fun y:int -> x+y) 12) in a 122
(Eval.Int 134)
```

## Planned features
### Syntax Sugar
* Recursive functions

### Typesystem
* Unit type
* Tuples
* Records
* Sum types
* Lists

### Recusive types
* Recusive types

### Polymorphism
* Hindley-Milner type system on System F

### Long term goals
* Linear types
* Typeclasses
* Algebaic effects
* Concurrency and Parallelism (API via algebraic effects, monads)
* Dependent types
* Compilation to:
  * Assembly
  * LLVM
  * C
  * Go
  * JS
* Virtual machine (maybe written in rust)

## Resources
* Types and programming languages (Benjamin C. Pierce)
* [Programming language Zoo](https://github.com/andrejbauer/plzoo/)
* Modern Compiler implementation in ML (Andrew Appel)