* stdlib fold:s variable shadowing
  * a successful map doesn't touch Binop Concat until eval?
    * is this due to it getting values of the wrong type?

-- TODO: Type system
* get 'Data' back
  * perhaps data doesn't cut it? would it be able to catch `Just Integer`, instead of just `Just`?
  * each Val should perhaps carry a struct/record specifying its type, along with other things of value

-- TODO: Pattern matching
  * allow multiple bindings of same name. how would env handle this? store bindings of the same name (and in the same scope), in an array?


-- KEY

-- MID
-- TODO: split expression on newline unless followed by (1) pipe or (2) indentation?
-- TODO: `filter` if-then-else without comma around predicate
-- TODO: case expression (specifically on maybe initially). depends on expressions reaching over lines
-- TODO: list index/at
-- TODO: modules/namespacing.

-- BACKLOG
-- TODO: string interpolation "#{intval}"
  * Could be LString ["val of a: ", (Atom "a"), "!"]?
-- TODO: repl catch eval error and continue
-- TODO: use `=` for function definition as well
-- TODO: types
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
