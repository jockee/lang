module ParserSpec where

import Data.String.Interpolate (i, iii)
import Parser
import Test.Hspec
import Types

s = showWithoutTypes

spec :: Spec
spec = describe "Parser" $ do
  it "true" $
    s (parseExpr "true") `shouldBe` s (PBool True)

  it "false" $
    s (parseExpr "false") `shouldBe` s (PBool False)

  it "int" $
    s (parseExpr "1") `shouldBe` s (PInteger 1)

  it "float" $
    s (parseExpr "1.1") `shouldBe` s (PFloat 1.1)

  it "list" $
    s (parseExpr "[1,2]") `shouldBe` s (PList anyTypeSig [PInteger 1, PInteger 2])

  it "handles whitespace" $
    s (parseExpr " [ 1 , 2 ] ") `shouldBe` s (PList anyTypeSig [PInteger 1, PInteger 2])

  it "double equals" $
    s (parseExpr "1 == 1") `shouldBe` s (Binop Eql (PInteger 1) (PInteger 1))

  it "not equals" $
    s (parseExpr "1 != 1") `shouldBe` s (Binop NotEql (PInteger 1) (PInteger 1))

  it "plus" $
    s (parseExpr "1 + 1") `shouldBe` s (Binop Add (PInteger 1) (PInteger 1))

  it "and" $
    s (parseExpr "1 && 1") `shouldBe` s (Binop And (PInteger 1) (PInteger 1))

  it "or" $
    s (parseExpr "1 || 1") `shouldBe` s (Binop Or (PInteger 1) (PInteger 1))

  it "string" $
    s (parseExpr "\"test\"") `shouldBe` s (PInterpolatedString [PString "t", PString "e", PString "s", PString "t"])

  it "ternary" $
    s (parseExpr "true ? 1 : 2") `shouldBe` s (PCase anyTypeSig (PBool True) [(PBool True, PInteger 1), (PBool False, PInteger 2)])

  it "if-then-else" $
    s (parseExpr "if true: 1 else 2") `shouldBe` s (PCase anyTypeSig (PBool True) [(PBool True, PInteger 1), (PBool False, PInteger 2)])

  it "let-in" $
    s (parseExpr "let x = 5: x + 1") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  it "lambda" $
    s (parseExpr "(x: x + 1)") `shouldBe` s (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1)))

  it "pipe to lambda" $
    s (parseExpr "5 |> (x: x + 1)") `shouldBe` s (Binop Pipe (PInteger 5) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

  it "lambda application" $
    s (parseExpr "(x: x + 1) 5") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  it "pipe to pipe" $
    s (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` s (Binop Pipe (Binop Pipe (PInteger 5) (Lambda anyTypeSig [Atom anyTypeSig "y"] (Binop Add (Atom anyTypeSig "y") (PInteger 1)))) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 2))))

  it "pipe partial application" $
    s (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` s (Binop Pipe (PList anyTypeSig [PInteger 1, PInteger 2]) (App (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))))

  it "nested lambda application" $
    s (parseExpr "(x: ((y: y + 1) x) + 2) 5") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (App (Lambda anyTypeSig [Atom anyTypeSig "y"] (Binop Add (Atom anyTypeSig "y") (PInteger 1))) (Atom anyTypeSig "x")) (PInteger 2))) (PInteger 5))

  it "nested lambda application 2" $
    s (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (App (Lambda anyTypeSig [Atom anyTypeSig "y"] (Binop Add (Atom anyTypeSig "y") (PInteger 1))) (PInteger 2)))) (PInteger 5))

  it "bind function to name in let-in" $
    s (parseExpr "let k = (x: x + 1): k 1") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "k"] (App (Atom anyTypeSig "k") (PInteger 1))) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

  it "partially applied lambda" $
    s (parseExpr "(x y: x + y) 1") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x", Atom anyTypeSig "y"] (Binop Add (Atom anyTypeSig "x") (Atom anyTypeSig "y"))) (PInteger 1))

  it "multiple argument lambda" $
    s (parseExpr "(x y: x + y + 1)") `shouldBe` s (Lambda anyTypeSig [Atom anyTypeSig "x", Atom anyTypeSig "y"] (Binop Add (Binop Add (Atom anyTypeSig "x") (Atom anyTypeSig "y")) (PInteger 1)))

  it "bind name" $
    s (parseExpr "a = 2") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (PInteger 2))

  it "assign list" $
    s (parseExpr "xs = [1]") `shouldBe` s (Binop Assign (Atom anyTypeSig "xs") (PList anyTypeSig [PInteger 1]))

  it "apply list to lambda" $
    s (parseExpr "(s: s) [1]") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "s"] (Atom anyTypeSig "s")) (PList anyTypeSig [PInteger 1]))

  it "function application" $
    s (parseExpr "(f b: x * b) (x: x*2) a") `shouldBe` s (App (App (Lambda anyTypeSig [Atom anyTypeSig "f", Atom anyTypeSig "b"] (Binop Mul (Atom anyTypeSig "x") (Atom anyTypeSig "b"))) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))) (Atom anyTypeSig "a"))

  it "pass list as function argument" $
    s (parseExpr "testFun [1]") `shouldBe` s (App (Atom anyTypeSig "testFun") (PList anyTypeSig [PInteger 1]))

  it "map function" $
    s (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` s (App (App (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))) (PList anyTypeSig [PInteger 1, PInteger 2]))

  it "partially applied map" $
    s (parseExpr "map (n: n * 2)") `shouldBe` s (App (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "n"] (Binop Mul (Atom anyTypeSig "n") (PInteger 2))))

  it "list concatenation" $
    s (parseExpr "[1] ++ [2]") `shouldBe` s (Binop Concat (PList anyTypeSig [PInteger 1]) (PList anyTypeSig [PInteger 2]))

  it "greater than" $
    s (parseExpr "1 > 0") `shouldBe` s (Cmp ">" (PInteger 1) (PInteger 0))

  it "dict" $
    s (parseExpr "{a: 1, b: 2}") `shouldBe` s (PDict anyTypeSig [(PDictKey "a", PInteger 1), (PDictKey "b", PInteger 2)])

  it "empty dict" $
    s (parseExpr "{}") `shouldBe` s (PDict anyTypeSig [])

  it "dict access on atom dot key" $
    s (parseExpr ".key exampledict") `shouldBe` s (DictAccess (Atom anyTypeSig "key") (Atom anyTypeSig "exampledict"))

  it "dict access on inline dot key" $
    s (parseExpr ".key {a: 1}") `shouldBe` s (DictAccess (PDictKey "key") (PDict anyTypeSig [(PDictKey "a", PInteger 1)]))

  it "dict access" $
    s (parseExpr "exampledict.key") `shouldBe` s (DictAccess (PDictKey "key") (Atom anyTypeSig "exampledict"))

  it "dict update" $
    s (parseExpr "{ {a: 1} | a:2 }") `shouldBe` s (PDictUpdate (PDict anyTypeSig [(PDictKey "a", PInteger 1)]) (PDict anyTypeSig [(PDictKey "a", PInteger 2)]))

  it "dict update alternative syntax (merge)" $
    s (parseExpr "{ {a: 1} | {a:2} }") `shouldBe` s (PDictUpdate (PDict anyTypeSig [(PDictKey "a", PInteger 1)]) (PDict anyTypeSig [(PDictKey "a", PInteger 2)]))

  it "dict dynamic key" $
    s (parseExpr "{ a => 1 }") `shouldBe` s (PDict anyTypeSig [(Atom anyTypeSig "a", PInteger 1)])

  it "dict update dynamic key" $
    s (parseExpr "{ {a: 1} | a => 2 }") `shouldBe` s (PDictUpdate (PDict anyTypeSig [(PDictKey "a", PInteger 1)]) (PDict anyTypeSig [(Atom anyTypeSig "a", PInteger 2)]))

  it "function definiton" $
    s (parseExpr "s x = x * 2") `shouldBe` s (Binop Assign (Atom anyTypeSig "s") (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Mul (Atom anyTypeSig "x") (PInteger 2))))

  it "internal function" $
    s (parseExpr "(HFI head [xs])") `shouldBe` s (HFI "head" (PList anyTypeSig [Atom anyTypeSig "xs"]))

  it "range" $
    s (parseExpr "(1..3)") `shouldBe` s (PRange anyTypeSig (PInteger 1) (PInteger 3))

  it "range to atom" $
    s (parseExpr "(1..a)") `shouldBe` s (PRange anyTypeSig (PInteger 1) (Atom anyTypeSig "a"))

  it "tuple" $
    s (parseExpr "(1, a)") `shouldBe` s (PTuple anyTypeSig [PInteger 1, Atom anyTypeSig "a"])

  it "destructuring tuple" $
    s (parseExpr "(a, b) = (1, 2)") `shouldBe` s (Binop Assign (PTuple anyTypeSig [Atom anyTypeSig "a", Atom anyTypeSig "b"]) (PTuple anyTypeSig [PInteger 1, PInteger 2]))

  describe "Type definition" $ do
    it "Binding definition" $
      show (parseExpr "a # Integer") `shouldBe` show (PTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [], typeSigReturn = IntType})

    it "Function definition" $
      show (parseExpr "a # Integer: Integer") `shouldBe` show (PTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [IntType], typeSigReturn = IntType})

  it "Type definition contains function" $
    s (parseExpr "a # (Integer: Integer): Integer")
      `shouldBe` s (PTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [FunctionType [IntType] IntType], typeSigReturn = IntType})

  it "Type definition contains multi-param function" $
    s (parseExpr "a # (b, c: b): b") -- HACK: to have to comma-separate function arguments here is a hack?
      `shouldBe` s (PTypeSig (TypeSig {typeSigName = Just "a", typeSigIn = [FunctionType [AnyType, AnyType] AnyType], typeSigReturn = AnyType}))

  it "Containing type variables" $
    s (parseExpr "at # a: Boolean") `shouldBe` s (PTypeSig (TypeSig {typeSigName = Just "at", typeSigIn = [AnyType], typeSigReturn = BooleanType}))

  it "Containing list" $
    s (parseExpr "at # [a]: a") `shouldBe` s (PTypeSig (TypeSig {typeSigName = Just "at", typeSigIn = [ListType AnyType], typeSigReturn = AnyType}))

  it "Containing list of concrete type" $
    s (parseExpr "at # [Integer]: a") `shouldBe` s (PTypeSig (TypeSig {typeSigName = Just "at", typeSigIn = [ListType IntType], typeSigReturn = AnyType}))

  it "Containing type constructor with type variable" $
    s (parseExpr "at # Integer: Maybe a") `shouldBe` s (PTypeSig (TypeSig {typeSigName = Just "at", typeSigIn = [IntType], typeSigReturn = TypeConstructorType "Maybe" AnyType}))

  describe "Pattern matching" $ do
    it "value constructor in function definition" $
      s (parseExpr "a None = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PDataConstructor "None" []] (PInteger 1)))

    it "value constructor taking argument in function definition" $
      s (parseExpr "a (Some b) = b") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PDataConstructor "Some" [Atom anyTypeSig "b"]] (Atom anyTypeSig "b")))

    it "empty list in function definition" $
      s (parseExpr "a [] = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PList anyTypeSig []] (PInteger 1)))

    it "integer in function definition" $
      s (parseExpr "a 1 = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PInteger 1] (PInteger 1)))

    it "tuple with integer function definition" $
      s (parseExpr "a ( b, c ) = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PTuple anyTypeSig [Atom anyTypeSig "b", Atom anyTypeSig "c"]] (PInteger 1)))

    it "all-atom tuple in function definition" $
      s (parseExpr "a ( b, c ) = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PTuple anyTypeSig [Atom anyTypeSig "b", Atom anyTypeSig "c"]] (PInteger 1)))

    it "dict full match" $
      s (parseExpr "a {b: c} = c") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PDict anyTypeSig [(PDictKey "b", Atom anyTypeSig "c")]] (Atom anyTypeSig "c")))

    xit "dict partial match" $
      s (parseExpr "a {b: c, ...} = c") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [PDict anyTypeSig [(PDictKey "b", Atom anyTypeSig "c")]] (Atom anyTypeSig "c")))

  describe "Modules" $
    it "parses module" $
      show (parseExpr "module A { 1 }") `shouldBe` show (Module "A" [PInteger 1])

  describe "Comment" $ do
    it "shebang" $
      show (parseExpr "#!/usr/bin/env langc") `shouldBe` show PNoop

    it "inline" $
      show (parseExpr "1 // comment") `shouldBe` show (PInteger 1)

    it "comment line" $
      show (parseExpr "// comment") `shouldBe` show PNoop

  describe "Expression separation" $ do
    it "can end on semicolon" $
      show (parseExprs "1;") `shouldBe` show [(PInteger 1)]

    it "semicolon splits" $
      show (parseExprs "1;a=2") `shouldBe` show [PInteger 1, Binop Assign (Atom anyTypeSig "a") (PInteger 2)]

    it "newline splits" $
      show (parseExprs "1\na=2") `shouldBe` show [PInteger 1, Binop Assign (Atom anyTypeSig "a") (PInteger 2)]

    it "newline doesn't split if preceded by infix operator" $
      show (parseExpr "a =\n 1") `shouldBe` show (Binop Assign (Atom anyTypeSig "a") (PInteger 1))

    it "avoids split before pipe" $
      s (parseExpr "5 \n |> (x: x + 1)") `shouldBe` s (Binop Pipe (PInteger 5) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

  describe "String interpolation" $ do
    it "just an integer" $
      show (parseExpr "\"a#{1}b\"") `shouldBe` show (PInterpolatedString [PString "a", PInteger 1, PString "b"])

    it "addition" $
      show (parseExpr "\"a#{1+1}b\"") `shouldBe` show (PInterpolatedString [PString "a", Binop Add (PInteger 1) (PInteger 1), PString "b"])

  describe "Cons list" $
    it "function definition" $
      show (parseExpr "a (x::xs) = 1") `shouldBe` show (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig [ConsList ["x", "xs"]] (PInteger 1)))

  describe "Data" $ do
    it "True and false" $
      show (parseExpr "data Bool = False | True") `shouldBe` show (PDataDefinition "Bool" [("False", []), ("True", [])])

    it "Value constructor - no args" $
      show (parseExpr "True") `shouldBe` show (PDataConstructor "True" [])

    it "Value constructor - multi args" $
      show (parseExpr "Rectangle 1.0 1.0 1.0") `shouldBe` show (PDataConstructor "Rectangle" [PFloat 1.0, PFloat 1.0, PFloat 1.0])

    it "Shape" $
      show (parseExpr "data Shape = Circle Float Float Float | Rectangle Float Float Float Float") `shouldBe` show (PDataDefinition "Shape" [("Circle", ["Float", "Float", "Float"]), ("Rectangle", ["Float", "Float", "Float", "Float"])])

  describe "Case expression" $ do
    it "two boolean cases" $
      s (parseExpr "case true: | true: 1 | false: 0") `shouldBe` s (PCase anyTypeSig (PBool True) [(PBool True, PInteger 1), (PBool False, PInteger 0)])

    it "handles more difficult expressions" $
      s (parseExpr "case (Some 1): \n| (Some x): 1 \n| None: 0") `shouldBe` s (PCase anyTypeSig (PDataConstructor "Some" [PInteger 1]) [(PDataConstructor "Some" [Atom anyTypeSig "x"], PInteger 1), (PDataConstructor "None" [], PInteger 0)])

  describe "Trait" $ do
    it "can create trait" $
      s (parseExpr "trait Mappable: | map # (a: b): a: b") `shouldBe` s (PTrait "Mappable" [PTypeSig (TypeSig {typeSigName = Just "map", typeSigIn = [FunctionType [AnyType] AnyType, AnyType], typeSigReturn = AnyType})] [])

    it "function definition in trait" $
      s (parseExpr "trait Mappable: | bap # (a: b): a: b | bap f xs = 1") `shouldBe` s (PTrait "Mappable" [(PTypeSig (TypeSig {typeSigName = Just "bap", typeSigIn = [FunctionType [AnyType] AnyType, AnyType], typeSigReturn = AnyType}))] [(Binop Assign (Atom anyTypeSig "bap") (Lambda anyTypeSig ([(Atom anyTypeSig "f"), (Atom anyTypeSig "xs")]) (PInteger 1)))])

    it "handles type variables" $
      s (parseExpr "trait Functor f: | fmap # (a: b), f a: f b") `shouldBe` s (PTrait "Functor" [PTypeSig (TypeSig {typeSigName = Just "fmap", typeSigIn = [FunctionType [AnyType] AnyType], typeSigReturn = TypeConstructorType "Functor" AnyType})] [])

  describe "Implementation" $ do
    it "can create implementation" $
      s (parseExpr "implement Mappable for Maybe: | map f a = None") `shouldBe` s (PImplementation "Mappable" "Maybe" [Binop Assign (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "f", Atom anyTypeSig "a"] (PDataConstructor "None" []))])

    it "can create implementation with multiple definitions" $
      s (parseExpr "implement Mappable for Maybe: | map _ None = None | map f (Some x) = f x") `shouldBe` s (PImplementation "Mappable" "Maybe" [Binop Assign (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "_", PDataConstructor "None" []] (PDataConstructor "None" [])), Binop Assign (Atom anyTypeSig "map") (Lambda anyTypeSig [Atom anyTypeSig "f", PDataConstructor "Some" [Atom anyTypeSig "x"]] (App (Atom anyTypeSig "f") (Atom anyTypeSig "x")))])
