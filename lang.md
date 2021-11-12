-- TODO: map as eval env
-- TODO: multiple argument lambda
-- TODO: pipe (reverse order function application?)
-- TODO: map lambdas on lists

-- TODO: Maybe
-- TODO: named functions / bound values
-- TODO: string interpolation
-- TODO: where clause
-- TODO: Static typing
-- TODO: Pattern matching
-- TODO: modules
-- TODO: REPL

maybeNum = Some 1 :  Maybe Integer // Maybe = Nothing | Some a

maybeNum
||> (n: n * 2) // bind lambda

// maybeNum
// ||> (n: n * Some 2) # ap lambda

[2,3]
|> map (n: n * 2)

// type classes
// '::' as cons opens up '#' for other use
// '|>' passes pre-operator value as last value (unless '#1' is included?)
// no significant whitespace (for indentation, maybe required for calling functions)
// no '$' function, but keep '.' for composition?
// pure?
// definitely functional. higher order functions
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
