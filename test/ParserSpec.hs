module ParserSpec where

import Parser
import Syntax
import Test.Hspec

s = showWithoutTypes

-- NOTE: the contents of this doesn't matter at all. `s` hides types
anyTypeSig = TypeSig {typeSigIn = [], typeSigReturn = AnyType}

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
    s (parseExpr "\"test\"") `shouldBe` s (PString "test")

  it "ternary" $
    s (parseExpr "true ? 1 : 2") `shouldBe` s (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "if-then-else" $
    s (parseExpr "if true then 1 else 2") `shouldBe` s (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "let-in" $
    s (parseExpr "let x = 5 in x + 1") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  -- it "let-in multiple args" $ do
  --   s (parseExpr "let x = 5\ny = 2\n in x + 1") `shouldBe` s (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

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
    s (parseExpr "let k = (x: x + 1) in k 1") `shouldBe` s (App (Lambda anyTypeSig [Atom anyTypeSig "k"] (App (Atom anyTypeSig "k") (PInteger 1))) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

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
    s (parseExpr "{ a => 1 }") `shouldBe` s (PDict anyTypeSig [((Atom anyTypeSig "a"), (PInteger 1))])

  it "dict update dynamic key" $
    s (parseExpr "{ {a: 1} | a => 2 }") `shouldBe` s (PDictUpdate (PDict anyTypeSig [((PDictKey "a"), (PInteger 1))]) (PDict anyTypeSig [((Atom anyTypeSig "a"), (PInteger 2))]))

  it "function definiton" $
    s (parseExpr "s x = x * 2") `shouldBe` s (Binop Assign (Atom anyTypeSig "s") (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Mul (Atom anyTypeSig "x") (PInteger 2))))

  it "nothing" $
    s (parseExpr "Nothing") `shouldBe` s PNothing

  it "just something" $
    s (parseExpr "Just 1") `shouldBe` s (PJust anyTypeSig (PInteger 1))

  it "internal function" $
    s (parseExpr "(InternalFunction head [xs])") `shouldBe` s (InternalFunction "head" (PList anyTypeSig [Atom anyTypeSig "xs"]))

  it "range" $
    s (parseExpr "[1..3]") `shouldBe` s (PRange anyTypeSig (PInteger 1) (PInteger 3))

  it "range to atom" $
    s (parseExpr "[1..a]") `shouldBe` s (PRange anyTypeSig (PInteger 1) (Atom anyTypeSig "a"))

  it "tuple" $
    s (parseExpr "( 1, a )") `shouldBe` s (PTuple anyTypeSig [PInteger 1, Atom anyTypeSig "a"])

  it "destructuring tuple" $
    s (parseExpr "( a, b ) = ( 1, 2 )") `shouldBe` s (Binop Assign (PTuple anyTypeSig [Atom anyTypeSig "a", Atom anyTypeSig "b"]) (PTuple anyTypeSig [PInteger 1, PInteger 2]))

  describe "Type definition" $ do
    it "Binding definition" $
      show (parseExpr "a :: Integer") `shouldBe` show (NamedTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [], typeSigReturn = IntType})

    it "Function definition" $
      show (parseExpr "a :: Integer -> Integer") `shouldBe` show (NamedTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [IntType], typeSigReturn = IntType})

  -- xit "Function definition contains function" $ do
  --   s (parseExpr "a :: (Integer -> Integer) -> Integer") `shouldBe` s (LTypeDef "a" TypeSig {typeSigIn=[Any], typeSigReturn=[Function [Type "Integer", Type "Integer"], Type "Integer"])

  describe "Pattern matching" $ do
    it "empty list in function definition" $
      s (parseExpr "a [] = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PList anyTypeSig [])]) (PInteger 1)))

    it "integer in function definition" $
      s (parseExpr "a 1 = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PInteger 1)]) (PInteger 1)))

    it "tuple with integer function definition" $
      s (parseExpr "a ( b, c ) = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PTuple anyTypeSig [(Atom anyTypeSig "b"), (Atom anyTypeSig "c")])]) (PInteger 1)))

    it "all-atom tuple in function definition" $
      s (parseExpr "a ( b, c ) = 1") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PTuple anyTypeSig [(Atom anyTypeSig "b"), (Atom anyTypeSig "c")])]) (PInteger 1)))

    it "dict full match" $
      s (parseExpr "a {b: c} = c") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PDict anyTypeSig [((PDictKey "b"), (Atom anyTypeSig "c"))])]) (Atom anyTypeSig "c")))

    xit "dict partial match" $
      s (parseExpr "a {b: c, ...} = c") `shouldBe` s (Binop Assign (Atom anyTypeSig "a") (Lambda anyTypeSig ([(PDict anyTypeSig [((PDictKey "b"), (Atom anyTypeSig "c"))])]) (Atom anyTypeSig "c")))

  describe "Modules" $ do
    it "parses module" $
      show (parseExpr "module A { 1 }") `shouldBe` show (Module "A" [PInteger 1])

  describe "Comment" $ do
    it "inline" $
      show (parseExpr "1 // comment") `shouldBe` show (PInteger 1)

    it "comment line" $
      show (parseExpr "// comment") `shouldBe` show PNoop

  describe "Expression separation" $ do
    it "semicolon splits" $
      show (parseExprs "1;a=2") `shouldBe` show [(PInteger 1), (Binop Assign (Atom anyTypeSig "a") (PInteger 2))]

    it "newline splits" $
      show (parseExprs "1\na=2") `shouldBe` show [(PInteger 1), (Binop Assign (Atom anyTypeSig "a") (PInteger 2))]

    it "avoids split before pipe" $
      s (parseExpr "5 \n  |> (x: x + 1)") `shouldBe` s (Binop Pipe (PInteger 5) (Lambda anyTypeSig [Atom anyTypeSig "x"] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))
