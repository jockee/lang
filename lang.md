# WIP

  - TODO: at function
    * foldl ends up with last argument first (??)
    * tuple destructuring seemingly not working for lambdas? has it ever
  - TODO: case expression (specifically on maybe initially). reqcleverexpsplit, reqtypesystem
    * when it's done we, `maybe` function
  - TODO: modules/namespacing.
    * nested

# KEY

  - TODO: PATTERN MATCHING
    - TODO: destructuring in named function arguments. #reqpatternmatching
      - TODO: dict full, dict partial
      - TODO: head and tail of list
      - TODO: tuple
  - TODO: (STATIC) TYPE SYSTEM #typesystem
    - TODO: if function is defined with set list in a position, that argument must be a list
    - TODO: handle lists and tuples as input types
    - TODO: handle tuple, dict, list
    - TODO: handle functions as input types
    - TODO: check return type on return - after `evalIn`
    - TODO: parse nested type definition
    - TODO: achieve polymorphism through type classes
    * optional type system. a full type system is too much work?
    * each Val should perhaps carry a struct/record specifying its type, along with other things of value
    * https://rosettacode.org/wiki/Type_detection#Kotlin
    - TODO: Type classes/traits:
      * Num (arithmetic)
      * Listable (fold as minimal complete definition)
      * Dictable (needs Listable and then fromList?)
  - TODO: SPLIT EXPRESSIONS on newline unless followed by (1) pipe or (2) indentation?. #cleverexpsplit
    * both lexeme and whitespace slurps newline
    - TODO: let in should be able to span multiple lines

# MID

  - TODO: `data` types ()

# BACKLOG

  - TODO: let-in multiple. #reqcleverexpsplit
    * depends on each other
  - TODO: sets
  - TODO: `filter` if-then-else without comma around predicate
  - TODO: string interpolation "#{intval}"
    * Could be LString ["val of a: ", (Atom "a"), "!"]?
  - TODO: repl catch eval error and continue
  - TODO: use `=` for function definition
  - TODO: use parens for tuples
  - TODO: where clause.
  - TODO: guard clause.
    fun x a
    | x > 1 = true
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
