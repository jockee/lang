# BUGS

  - BUG: VARIABLE SHADOWING: stdlib fold:s variable shadowing #varshadow
      * a successful map doesn't touch Binop Concat until eval?
        * is this due to it getting values of the wrong type?
# KEY

  - TODO: split expression on newline unless followed by (1) pipe or (2) indentation?. #cleverexpsplit
    - TODO: let in should be able to span multiple lines
  - TODO: Type system #typesystem
    - TODO: complain if receiving wrong args
    - TODO: parse nested type definition
    - TODO: `data` types ()
    * optional type system. a full type system is too much work?
    * each Val should perhaps carry a struct/record specifying its type, along with other things of value
    * https://rosettacode.org/wiki/Type_detection#Kotlin
    - TODO: Type classes/traits:
      * Listable (fold as minimal complete definition)
      * Dictable (needs Listable and then fromList?)
  - TODO: Pattern matching #pattermatching
    * allow multiple bindings of same name. how would env handle this? store bindings of the same name (and in the same scope), in an array?
    - TODO: destructuring in named function arguments. #reqpatternmatching

# MID

  - TODO: case expression (specifically on maybe initially). reqcleverexpsplit, reqtypesystem
    * when it's done we, `maybe` function
  - TODO: list index/at
  - TODO: modules/namespacing.

# BACKLOG

  - TODO: dynamic dict keys. enables toDict
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
