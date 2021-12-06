# KEY

# MID

  - monoid for use with `join` for instance
  - function composition
    - pipes (maybe only for pipes?)
    - pipe always constructs new lambda?
  - string split
  - function from implementation overrides trait function (efficient `length` for strings, for instance)

  ## USABILITY
  - TODO: sockets

  ## PARSING
  - TODO: too much needs to be wrapped in parens
    * `| Some x: (print "hello #{x}") ` can't be `| Some x: print "hello #{x}"`

  - TODO: Type classes/traits:
    * Num? (arithmetic)
      * Add Num constraint to function doing addition

  - TODO: PATTERN MATCHING
    - TODO: dict partial - spread operator
    - TODO: move pattern matching check out of extend

  - TODO: (STATIC) TYPE SYSTEM #typesystem
    - TODO: if function is defined with set list in a position, that argument must be a list for all definitions
    - TODO: handle lists and tuples as input types
    - TODO: handle tuple, dict, list
    - TODO: handle functions as input types
    - TODO: check return type on return - after `evalIn`
    - TODO: parse nested type definition


# BACKLOG

  - TODO: sets
  - TODO: where clause
  - TODO: guard clause
    fun x a
    | x > 1 = true
  - TODO: `filter` if-then-else without parens around predicate
  - TODO: nested modules

# REFACTORING

* ReaderT, StateT? MTL
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
