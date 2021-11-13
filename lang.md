  -- TODO: implement map in 'lang' (stdlib). begin evalInEnv with reading in standard library
-- TODO: Read stdlib from file(s)

-- TODO: LConcat as binop in table?



-- TODO: partial mapfn (to enable pipe)
-- TODO: general case of partially applied named function? don't make map anything special. do so by having everything in stdlib? the exposed fold should be a stdlib one. how about partial application of regular operators? not initially? (x: x + 1) is as good as (+1)
-- TODO: dict/record/map
-- TODO: list index/at
-- TODO: pretty syntax for bound functions
-- TODO: Ord (GT LT EQ)

-- TODO: Maybe
-- TODO: string concatenation ("ok" + "ok")
-- TODO: string interpolation

-- TODO: where clause
-- TODO: Pattern matching
-- TODO: case expression
-- TODO: Static typing

-- TODO: modules
-- TODO: replace string with text?

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
