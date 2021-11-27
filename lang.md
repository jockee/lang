# KEY

  - TODO: Lib location when shebang calling. how to keep track?
  - TODO: too much needs to be wrapped in parens
    * `| Some x: (print "hello #{x}") ` can't be `| Some x: print "hello #{x}"`
  - TODO: parse file doesn't handle end of file well. can't currently end on semicolon
  - TODO: handle haskell error (readfile)
  - TODO: remove the need for semicolons
    * fix `either`

  - TODO: sockets

  - TODO: Type classes/traits:
    * `length` for strings, dict, tuple, list (Foldable)
    * Num? (arithmetic)
      * Add Num constraint to function doing addition

  - TODO: modules/namespacing.
    * nested

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
    - TODO: achieve polymorphism through type classes:
      * optional type system. a full type system is too much work?
      * each Val should perhaps carry a struct/record specifying its type, along with other things of value
      * https://rosettacode.org/wiki/Type_detection#Kotlin

# MID

# BACKLOG

  - TODO: sets
  - TODO: where clause
  - TODO: guard clause
    fun x a
    | x > 1 = true
  - TODO: `filter` if-then-else without parens around predicate
  - TODO: replace string with T.Text?

# NOTES

-- NOTE: ON SCOPE LEAK: if it still leaks, look at hash key generation. maybe pass 'scopeId' to App (of Expr)?
-- NOTE: remove AtomType - it's AnyType?
-- NOTE: module braces syntax?

-- COULD BE MOVED TO STDLIB
* dict update merger. could it, though? how would we modify the dict?

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
// no significant whitespace (for indentation, maybe required for calling functions)
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
