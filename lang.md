-- KEY
-- TODO: looks like every internal variable leaks into global scope <- probably what's not allowing us to replace foldInternal with fold in stdlib
  * if var only exists in scope, remove it after application
  * currently we're only adding to scope.
  * nested named function could definitely have the same argument names. those shouldn't be mixed up. see how `unique` uses `includes?`
  * when we're as deeply nested as possible, we need to step back up and remove scope?
  * how to know when to release var?
  * not just global scope, but nested scope as well (let-in in lambda, for instance)

  * if it still leaks, look at hash key generation

-- MID
-- TODO: split expression on newline unless followed by (1) pipe or (2) indentation?
-- TODO: `filter` if-then-else without comma around predicate
-- TODO: Maybe
  * maybe function. requires type knowledge? if is a Nothing
  * case expression. after maybe. depends on expressions reaching over lines
  * list index/at
-- TODO: string interpolation "#{intval}"
  * Could be LString ["val of a: ", (Atom "a"), "!"]?

-- BACKLOG
-- TODO: sort function
-- TODO: repl catch eval error and continue
-- TODO: use `=` for function definition as well
-- TODO: types
-- TODO: Pattern matching
  * allow multiple bindings of same name
-- TODO: where clause. reqscope
-- TODO: guard clause.
  fun x a
  | x > 1 = true
-- TODO: Static typing
  * i guess inference is hard, so everything needs to be typed?

-- TODO: modules/namespacing. reqscope
-- TODO: replace string with T.Text?

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
