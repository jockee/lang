* get 'Data' back
* remove special fold parser
* use stdlib fold in stdlib
  * BLOCK: map through fold leaks

* `min [12,3,4,54, -1]` fails

-- KEY

-- MID
-- TODO: split expression on newline unless followed by (1) pipe or (2) indentation?
-- TODO: `filter` if-then-else without comma around predicate
-- TODO: case expression (specifically on maybe initially). depends on expressions reaching over lines
-- TODO: list index/at
-- TODO: string interpolation "#{intval}"
  * Could be LString ["val of a: ", (Atom "a"), "!"]?
-- TODO: modules/namespacing.

-- BACKLOG
-- TODO: repl catch eval error and continue
-- TODO: use `=` for function definition as well
-- TODO: types
-- TODO: Pattern matching
  * allow multiple bindings of same name
-- TODO: where clause.
-- TODO: guard clause.
  fun x a
  | x > 1 = true
-- TODO: Static typing
  * i guess inference is hard, so everything needs to be typed?

-- TODO: replace string with T.Text?


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
