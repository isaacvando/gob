# Gob 🐱

Gob is a very small stack based programming language.

Every program is a sequence of values that operate on a single global stack. For example, the following program evaluates to 9:
```
# examples/sum.gob
4 5 +
```
```bash
$ gob examples/sum.gob --debug
 | 4 5 +
4 | 5 +
4 5 | +
9 | 
9
```

Everything on the left of the `|` is the current state of the stack and everything on the right is the current state of the program. First `4` is pushed on the stack, followed by `5`, at which point `+` pops both numbers off the stack, sums them, and pushes the answer back on the stack. At that point the program is complete so the execution finishes by outputting `9`.

More examples can be found in [./examples](./examples). 

## Features
The data types Gob currently supports are strings, non-negative integers, booleans, and quotations. 
```
"Hello World!"
1729
true
[10 445 +]
``` 

The full list of supported functions can be found in the code.

### Piping programs
Due to the nature of stack based programs, you can compose two together simply by concatenating them. Gob takes advantage of this by allowing you to pipe a program into the interpreter and combine it with the program read from a file. 

For example, the factorial program reads its argument from stdin
```bash
$ echo 7 | gob examples/factorial.gob --pipe
5040
```

This first argument can be any program, so equivalently we can do:
```bash
$ echo [3 4 +] apply | gob examples/factorial.gob --pipe
5040
```

I think this idea could be very useful for shell scripting because of the ability to easily compose programs directly on the command line. I have not explored it much however, and right now practical benefits are limited by Gob's very small feature set.

### Defs
Gob programs consist of any number of definitions followed by the body of the program. Defs are useful for readability and easily implementation recursion. When a def is referenced in the body of the program, the reference to the def is replaced by its contents. For example, factorial can be implemented like this:
```
# examples/factorialDefs.gob
fact: 1 - isZero [drop] [dup dig * swap fact] branch apply
isZero: dup 0 =

dup fact
```
```bash
$ echo 10 | gob examples/factorialDefs.gob --pipe
3628800
```

### CLI
To see the rest of the CLI options, run 
```
gob --help
```

## Building from source
Gob's interpreter is implemented in [Roc](https://roc-lang.org), a delightful functional language.

To use the language, you will need the [Roc compiler](https://github.com/roc-lang/roc).

To build the interpreter, run 
```bash
roc build main.roc --output ./gob
```

## Inspiration
Gob was originally inspired by the excellent talk ["Concatenative programming and stack-based languages](https://youtu.be/umSuLpjFUf8?si=SV1c_Zwc5F4-cPJS) by Douglas Creager at Strange Loop 2023.
