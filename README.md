# Compilers
Spring 2022 Compilers project

Isra Ali
isra.hamid_ali@tufts.edu

Grenwolyn Edgar
grendolynedgar@tufts.edu

Randy Price
edward.price@tufts.edu

Chris Xiong
lxiong01@tufts.edu

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
