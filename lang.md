# WIP

 - why are functions applied multiple times? fold break span. `evalIn`s spread out without keeping evaled value? `toExpr`?
 - nothing is ever executed more than once
 - replace map with fmap

# KEY

 - throw error in `head!`
 - import module "non-qualified"

# MID

  - monoid for use with `join` for instance
  - function composition
    - pipes (maybe only for pipes?)
    - pipe always constructs new lambda?
      - zero argument lambda? if not: can we easily judge whether it should be a lambda or a binop pipe returned?
  - string split
  - function from implementation overrides trait function (efficient `length` for strings, for instance)
  - pattern matching
    - dict partial - spread operator
    - with specific values `{[], "world", 42}`. currently sets values, but doesn't `throw` non mismatched specific

  ## USABILITY
  - TODO: sockets, for tcp server

  ## PARSING
  - TODO: Type classes/traits:
    * Num? (arithmetic)
      * Add Num constraint to function doing addition

  - TODO: (STATIC) TYPE SYSTEM #typesystem
    - TODO: if function is defined with set list in a position, that argument must be a list for all definitions
    - TODO: handle lists and tuples as input types
    - TODO: handle tuple, dict, list
    - TODO: handle functions as input types
    - TODO: check return type on return - after `evalIn`
    - TODO: parse nested type definition

# BACKLOG

  - pin operator
  - `+` for concatenation as well?
  - TODO: sets
  - TODO: where clause
  - TODO: guard clause
    fun x a
    | x > 1 = true

# IDEAS

* should data definitions use `type` instead of `data`? mixes up what a 'type constructor' is
* read in functions from HFI straight into std lib?
  * difficult to do into modules?

# REFACTORING

* strings seem very slow
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
