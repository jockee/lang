# WIP

  - parser: allow newline before block ending `}`
  - when specifying module, the atom needs to exist in that specific module
  - runTests in UnitTest module: Exception: EvalException "Atom a does not exist in scope"

# KEY

  - function composition
    - pipes (maybe only for pipes?)
    - pipe always constructs new lambda?
      - zero argument lambda? if not: can we easily judge whether it should be a lambda or a binop pipe returned?
      - we cant know in parsing whether functions are fully applied
      - when evaluating, we can return a lambda'd pipe if partial?
      - composing with pipes - require parens?
      - it cant be just a regular apply, can it?

# MID

  - read in functions from HFI straight into std lib?
    - difficult to do into modules?
  - import module "non-qualified"
  - monoid for use with `join` for instance
  - function from implementation overrides trait function (efficient `length` for strings, for instance)
  - Type classes/traits:
    * Num? (arithmetic)
      * Add Num constraint to function doing addition

  - (STATIC) TYPE SYSTEM [typesystem]
    - if function is defined with set list in a position, that argument must be a list for all definitions
    - handle lists and tuples as input types
    - handle tuple, dict, list
    - handle functions as input types
    - check return type on return - after `evalIn`
    - parse nested type definition
  - sockets, for tcp server [usability]

# BACKLOG

  - looks like bound things are evaluated both on definition and call. test with a print
  - multiple expressions (for IO) in brackets?
  - ghcjs
  - extend STDLIB
    - flatMap, flatten
    - testing lib (assert, refute, assertEqual)
      ```
        ...
        ----------------------------------------------------------------------
        Ran 3 tests in 0.000s

        OK
      ```
    - coercion (to string)
    - regexp
    - readline
    - struct?
    - date/time
    - randomness
    - encoding base64/md5 etc
  - pin operator
  - TODO: sets
    // intersection
    // difference
  - TODO: where clause. how to group?
  - TODO: guard clause
    fun x a
    | x > 1 = true
  - debug({x}) -> {x: 1.2} punning
    - punning character = {name, role} -> ch

# IDEAS

* should data definitions use `type` instead of `data`? mixes up what a 'type constructor' is, but
  that's only internal cruft
* built in testing lib?
* how to handle multi-expression blocks? surrounding anything in braces makes it
  multi-expressionable? ie, doesn't have to be built-in for any expressions?
    asd ? {
      do1
      do2
    } : do3
- pattern matching
  - dict partial - spread operator - currently always matching on partial
- remove 'let' keyword from let bindings `a = 1, b = 2: b + a`


# REFACTORING

* strings seem very slow
* ReaderT, StateT? MTL (start out with readert as it's the most basic?) -- not sure anything like
  this really fits this project
* Boolean could be defined as `data Boolean = True | False`

# NOTES

----

maybeNum = Some 1 :  Maybe Integer // Maybe = Nothing | Some a

maybeNum
||> (n: n * 2) // bind lambda

// maybeNum
// ||> (n: n * Some 2) # ap lambda

[2,3]
|> map (n: n * 2)

// type classes
// '::' as cons opens up ':' for other use
// no significant whitespace (for indentation, maybe required for cAlling functions)
// no '$' function, but keep '.' for composition?
// pure?
// very basic type inference. passing [Integer] to `map`, `map` should know it's getting integers
// where clauses
// Not point-free initially, unless it comes for free
// either:
  // instead of Maybe, something can be `Integer?`; or
  // allow question marks ending function names

isPositive? : Integer -> String
isPositive? 0 = "Unclear. It's zero"
isPositive? n = isNegative? ? "No." : "Yes, it's #{n}"
where isNegative? n = n < 0
