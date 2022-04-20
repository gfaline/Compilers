# Compilers
Spring 2022 Compilers project

Isra Ali
isra.hamid_ali@tufts.edu

Gwendolyn Edgar
gwendolyn.edgar@tufts.edu

Randy Price
edward.price@tufts.edu

Chris Xiong
lxiong01@tufts.edu

## Requirements

Required tools are similar to that of MicroC. Below is a pessimistic estimate of versions that will
work:

```
OCaml >4.0, opam >2, LLVM for OCaml >8.0
```

In addition, you need a relatively recent version of Bash to run the scripts. On macOS, you can use
Homebrew or a similar package manager to install a recent version of bash.

## Building & Testing

The following instruction has been tested on the following platforms:

 - Gentoo Linux ~amd64, OCaml 4.13.1, opam 2.1.2, LLVM 13.0.01

Assuming you are already in the `propeller` subfolder, use the following command to build the
compiler:

```
make
```

Use the following command to run all supplied tests:

```
make test
```

In addition, use `test_*` targets to run tests in a certain category (e.g. `make test_parser`
runs all tests for the parser).

Tests are done using a simple testing utility script (`testutil.sh`). Three different aspects of
execution result can be checked against a given standard: standard output, standard error and
return code. The first two can be ignored if needed. Since the compiler exits with non-zero return
code if an invalid program is fed to it, it can be used to verify test cases in which the compilation is expected to fail.

A description of each test is included at the top of its corresponding .pr source file.

## Compiler Invocation

A script has been provided for generating executable in one step. Use `./prc.sh <source.pr>` to create
an executable for `source.pr`. The executable created will be named `source.out` (or `source.exe` on
Windows), located in the same directory as `source.pr`.

Use `./prc.sh -k <source.pr>` to keep all intermediate files.

## Language Overview

Since Propeller has C-like syntax, we borrowed several simple grammar rules
and their corresponding OCaml expressions from MicroC. Rather than list
every little language feature or grammar rule inspired by the MicroC
compiler, it's probably easier to list the language features unique to 
to Propeller, and to assume everything else was inspired by MicroC (unless otherwise noted):

- OBJECTS / REACTIVE PROGRAMMING
    - In addition to a list of variable declarations and function
      declarations, a Propeller program also includes a list of
      user-defined type (object) declarations
    - The PERIOD operator . , used to initialize properties of objects
      retrieve their values, and bind functions to them
    - Binding/unbinding of functions to object properties
- CONTROL FLOW
    - Removal of Blocks. Control flow/looping statements are always followed
      by a list of one or more statements (we use the stmt_list nonterminal
      instead of allowing a list of stmts to reduce to another stmt)
    - stmt lists associated with if/elif/else/while/for must be wrapped
      in braces {}
    - exprs of if/elif/while need not be enclosed in parentheses
    - if/elif*/else statements instead of simple if/else
        - We consulted this stack overflow post to implement our rules
          for if/elif/else:
          https://stackoverflow.com/questions/34751697/trouble-parsing-elif-using-ply
    - different syntax and indented application of for loops
- PRIMITIVE / BUILT-IN FUNCTIONALITY 
    - primitive string (str) type
    - primitive immutable list type (e.g. int list, str list, int list list)
    - list indexing
    - list literals
    - built-in modulus operator
    - built-in xor operator
- OTHER
    - different ID-naming convention (the scanner will let you know when
      an ID is invalid!)
    - several revised/tidied printing functions for vdecls, odecls, and fdecls
- TESTING
    - A simple testing framework is used. See `testutil.sh`.

## Tests

A broad range of tests for different features of Propeller can be found in
`propeller/tests/exttest`.

* Note for the "Extended Testsuite" deliverable: since lists, objects and bindings aren't
ready for testing yet, current tests only focus on basic features. Features tested that
are not found in MicroC include: modulo operator, elif branches, and break / continue
statements.

Here's a list of included tests and a short description for each for your convenience:

```
fail-badreturn.pr         | Semantic error: returning a float in a function that should return int.
fail-breakwithoutloop.pr  | Semantic error: break statement outside a loop
fail-for-varid-reused.pr  | Semantic error: trying to use an already declared variable as for-loop variable.
test-break.pr             | Simple test for the break statement.
test-cont1.pr             | Simple test for the continue statement.
test-cont2.pr             | Variant of the simple continue test: continuing just before loop predicate becomes false.
test-expr.pr              | Print the result of a simple integer expression, and return 0.
test-for-nested.pr        | Basic test for nested for-loops
test-for.pr               | Basic test for for-loop
test-functioncall.pr      | Testing basic function calls.
test-if.pr                | Test a basic if statement
test-ifelif1.pr           | Tests variable assignment and and if, elif, else statement to print on elif (part 1)
test-ifelif2.pr           | Tests variable assignment and and if, elif, else statement to print on elif (part 2)
test-ifelif3.pr           | Tests variable assignment and and if, elif, else statement to print on elif (part 3)
test-mod.pr               | Test the semantics of the modulo operator -- we use the "truncated" definition
test-varprint.pr          | A simple program that prints a variable and returns 0.
test-while.pr             | Simple test for the while statement.
```
