-- KEY
-- TODO: looks like every internal variable leaks into global scope <- probably what's not allowing us to replace foldInternal with fold in stdlib
  -- TODO: if var only exists in scope, remove it after application
  -- TODO: currently we're only adding to scope.
  when we're as deeply nested as possible, we need to step back up and remove scope?



how to know when to release var?
* not just global scope, but nested scope as well (let-in in lambda, for instance)



-- TODO: repl catch eval error and continue
-- TODO: do we want/need string keys in dicts?


-- MID
-- TODO: newline can't be breaking every expression. consider piping
-- TODO: print
-- TODO: `filter` if-then-else without comma around predicate and then
-- TODO: pretty syntax for bound functions: `empty xs = xs == []`?

-- TODO: Maybe
  -- TODO: list index/at
-- TODO: string interpolation "#{intval}"

-- BACKLOG
-- TODO: where clause
-- TODO: Pattern matching
-- TODO: case expression
-- TODO: Static typing

-- TODO: modules/namespacing
-- TODO: replace string with T.Text?

-- COULD BE MOVED TO STDLIB
* dict update merger

----

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
