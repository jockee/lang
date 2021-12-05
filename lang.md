* we need to package lambdas whole env up (not just arguments) when we pass it somewhere
* we do need to evaluate arguments to pattern match, so might as well pass them

* as other functions and modules are stateless, we can scope them according to name
  ^ may be called several times, which might be tricky in that situation



















* partial application needs to pre-prepare a scope

* extend of variables needs to be able to look into the scope above

λ> evals $ parseExprs "filter' f xs = f xX; s xX = filter' (a: a < xX) 1; s 1"
looking for s in module
looking for filter' in function "s"
looking for f in function "filter'"
looking for a in lambda
looking for xX in lambda
looking for xX in lambda
(BoolVal False)
it :: Val

it's not returning to function scopetype after lambda call



* xX shouldn't exist in the env its trying to extend

SCOPING:

* when calling a function:
  * drop all local scope?

  lambda:
    * its own scope
    * scope of module in which it was created
    * scope of module in which it's called
    * the scope of the function in which it is called

    on call:
      * don't allow it to take scope from SFunction in which it was created
      * create its own scope


  function:
    * its own scope
    * scope of module in which it was created

    on call:
      * don't allow it to take scope from SFunction and SLambda
      * don't allow it to see SModule, unless it's its creation module
      * create its own scope

  module:
    * its own scope

    on call, hide:
      * other modules





* http request - slime by using haskell package?







# KEY

  - "nested functions can use the same variable name"

# MID

  - implement fold for dict, string and list
    - monoid, empty, concat?
  - string split
  - function from implementation overrides trait function

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

* slowness from pushing env into lambda (and doing so inefficiently). avoid storing 'stdlib' module?
  would have to make it a module that can be called without prefixing `Stdlib.`
* ReaderT, StateT? MTL

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
