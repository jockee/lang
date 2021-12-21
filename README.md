# lang

```swift
sort [] = []
sort (y | xs) = let smaller = filter (a: a <= y) xs, // pattern matching on cons, let binding
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
  |> find ((a, b): b == i) // lambda
  ||> first // fmap pipe
```
