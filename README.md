# lang

A functional language. Soon to be statically typed.

```swift
a = %{a: 1} // dict
b = [1, 2] // list
c = (1, 2) // tuple
```

```swift
trait Functor f { // create trait (type class)
  map (a: b), f a => f b // definition
}

implement Functor for Maybe { // implements trait
  map _ None = None
  map fm (Some x) = Some (fm x)
}
```

```swift
sort [] = []
sort (y | xs) = let smaller = filter (a: a <= y) xs, // pattern matching and destructuring with cons, let binding, lambda
                    larger = filter (a: a > y) xs:
                  sort smaller + [y] + sort larger // concat
``` 

```swift
maybe default f None = default
maybe default f (Some x) = Some (f x)
```

```swift
either ef of e = case e { // case expression
  Err x: Err (ef x) // Result type
  Ok x: Ok (of x)
}
```

```swift
at _ [] = None
at i xs = // multiple definitions
  zip xs [0 .. length xs] // function application, range
  |> find ((a, b): b == i) // find is total and returns a Maybe
  ||> first // fmap pipe
```

Written in Haskell using Megaparsec.
