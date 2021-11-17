module ParserSpec where

import Parser
import Syntax
import Test.Hspec

-- NOTE: the contents of this doesn't matter at all. `showWithoutTypes` hides types
anyTypeSig = TypeSig {typeSigIn = [], typeSigReturn = AnyType}

spec :: Spec
spec = describe "Parser" $ do
  it "true" $ do
    showWithoutTypes (parseExpr "true") `shouldBe` showWithoutTypes (PBool True)

  it "false" $ do
    showWithoutTypes (parseExpr "false") `shouldBe` showWithoutTypes (PBool False)

  it "int" $ do
    showWithoutTypes (parseExpr "1") `shouldBe` showWithoutTypes (PInteger 1)

  it "float" $ do
    showWithoutTypes (parseExpr "1.1") `shouldBe` showWithoutTypes (PFloat 1.1)

  it "list" $ do
    showWithoutTypes (parseExpr "[1,2]") `shouldBe` showWithoutTypes (PList anyTypeSig [PInteger 1, PInteger 2])

  it "handles whitespace" $ do
    showWithoutTypes (parseExpr " [ 1 , 2 ] ") `shouldBe` showWithoutTypes (PList anyTypeSig [PInteger 1, PInteger 2])

  it "double equals" $ do
    showWithoutTypes (parseExpr "1 == 1") `shouldBe` showWithoutTypes (Binop Eql (PInteger 1) (PInteger 1))

  it "not equals" $ do
    showWithoutTypes (parseExpr "1 != 1") `shouldBe` showWithoutTypes (Binop NotEql (PInteger 1) (PInteger 1))

  it "plus" $ do
    showWithoutTypes (parseExpr "1 + 1") `shouldBe` showWithoutTypes (Binop Add (PInteger 1) (PInteger 1))

  it "and" $ do
    showWithoutTypes (parseExpr "1 && 1") `shouldBe` showWithoutTypes (Binop And (PInteger 1) (PInteger 1))

  it "or" $ do
    showWithoutTypes (parseExpr "1 || 1") `shouldBe` showWithoutTypes (Binop Or (PInteger 1) (PInteger 1))

  it "string" $ do
    showWithoutTypes (parseExpr "\"test\"") `shouldBe` showWithoutTypes (PString "test")

  it "ternary" $ do
    showWithoutTypes (parseExpr "true ? 1 : 2") `shouldBe` showWithoutTypes (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "if-then-else" $ do
    showWithoutTypes (parseExpr "if true then 1 else 2") `shouldBe` showWithoutTypes (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "let-in" $ do
    showWithoutTypes (parseExpr "let x = 5 in x + 1") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  -- it "let-in multiple args" $ do
  --   showWithoutTypes (parseExpr "let x = 5\ny = 2\n in x + 1") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  it "lambda" $ do
    showWithoutTypes (parseExpr "(x: x + 1)") `shouldBe` showWithoutTypes (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1)))

  it "pipe to lambda" $ do
    showWithoutTypes (parseExpr "5 |> (x: x + 1)") `shouldBe` showWithoutTypes (Binop Pipe (PInteger 5) (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

  it "lambda application" $ do
    showWithoutTypes (parseExpr "(x: x + 1) 5") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))) (PInteger 5))

  it "pipe to pipe" $ do
    showWithoutTypes (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` showWithoutTypes (Binop Pipe (Binop Pipe (PInteger 5) (Lambda anyTypeSig [(Atom anyTypeSig "y")] (Binop Add (Atom anyTypeSig "y") (PInteger 1)))) (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 2))))

  it "pipe partial application" $ do
    showWithoutTypes (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` showWithoutTypes (Binop Pipe (PList anyTypeSig [(PInteger 1), (PInteger 2)]) (App (Atom anyTypeSig "map") (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))))

  it "nested lambda application" $ do
    showWithoutTypes (parseExpr "(x: ((y: y + 1) x) + 2) 5") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (App (Lambda anyTypeSig [(Atom anyTypeSig "y")] (Binop Add (Atom anyTypeSig "y") (PInteger 1))) (Atom anyTypeSig "x")) (PInteger 2))) (PInteger 5))

  it "nested lambda application 2" $ do
    showWithoutTypes (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (App (Lambda anyTypeSig [(Atom anyTypeSig "y")] (Binop Add (Atom anyTypeSig "y") (PInteger 1))) (PInteger 2)))) (PInteger 5))

  it "bind function to name in let-in" $ do
    showWithoutTypes (parseExpr "let k = (x: x + 1) in k 1") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "k")] (App (Atom anyTypeSig "k") (PInteger 1))) (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Add (Atom anyTypeSig "x") (PInteger 1))))

  it "partially applied lambda" $ do
    showWithoutTypes (parseExpr "(x y: x + y) 1") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "x"), (Atom anyTypeSig "y")] (Binop Add (Atom anyTypeSig "x") (Atom anyTypeSig "y"))) (PInteger 1))

  it "multiple argument lambda" $ do
    showWithoutTypes (parseExpr "(x y: x + y + 1)") `shouldBe` showWithoutTypes (Lambda anyTypeSig [(Atom anyTypeSig "x"), (Atom anyTypeSig "y")] (Binop Add (Binop Add (Atom anyTypeSig "x") (Atom anyTypeSig "y")) (PInteger 1)))

  it "bind name" $ do
    showWithoutTypes (parseExpr "a = 2") `shouldBe` showWithoutTypes (Binop Assign (Atom anyTypeSig "a") (PInteger 2))

  it "assign list" $ do
    showWithoutTypes (parseExpr "xs = [1]") `shouldBe` showWithoutTypes (Binop Assign (Atom anyTypeSig "xs") (PList anyTypeSig [(PInteger 1)]))

  it "apply list to lambda" $ do
    showWithoutTypes (parseExpr "(s: s) [1]") `shouldBe` showWithoutTypes (App (Lambda anyTypeSig [(Atom anyTypeSig "s")] (Atom anyTypeSig "s")) (PList anyTypeSig [(PInteger 1)]))

  it "function application" $ do
    showWithoutTypes (parseExpr "(f b: x * b) (x: x*2) a") `shouldBe` showWithoutTypes (App (App (Lambda anyTypeSig [(Atom anyTypeSig "f"), (Atom anyTypeSig "b")] (Binop Mul (Atom anyTypeSig "x") (Atom anyTypeSig "b"))) (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))) (Atom anyTypeSig "a"))

  it "pass list as function argument" $ do
    showWithoutTypes (parseExpr "testFun [1]") `shouldBe` showWithoutTypes (App (Atom anyTypeSig "testFun") (PList anyTypeSig [(PInteger 1)]))

  it "map function" $ do
    showWithoutTypes (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` showWithoutTypes (App (App (Atom anyTypeSig "map") (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Mul (Atom anyTypeSig "x") (PInteger 2)))) (PList anyTypeSig [(PInteger 1), (PInteger 2)]))

  it "partially applied map" $ do
    showWithoutTypes (parseExpr "map (n: n * 2)") `shouldBe` showWithoutTypes (App (Atom anyTypeSig "map") (Lambda anyTypeSig [(Atom anyTypeSig "n")] (Binop Mul (Atom anyTypeSig "n") (PInteger 2))))

  it "list concatenation" $ do
    showWithoutTypes (parseExpr "[1] ++ [2]") `shouldBe` showWithoutTypes (Binop Concat (PList anyTypeSig [(PInteger 1)]) (PList anyTypeSig [(PInteger 2)]))

  it "greater than" $ do
    showWithoutTypes (parseExpr "1 > 0") `shouldBe` showWithoutTypes (Cmp ">" (PInteger 1) (PInteger 0))

  it "dict" $ do
    showWithoutTypes (parseExpr "{a: 1, b: 2}") `shouldBe` showWithoutTypes (PDict anyTypeSig [((PDictKey "a"), (PInteger 1)), ((PDictKey "b"), (PInteger 2))])

  it "empty dict" $ do
    showWithoutTypes (parseExpr "{}") `shouldBe` showWithoutTypes (PDict anyTypeSig [])

  it "dict access on atom dot key" $ do
    showWithoutTypes (parseExpr ".key exampledict") `shouldBe` showWithoutTypes (DictAccess (Atom anyTypeSig "key") (Atom anyTypeSig "exampledict"))

  it "dict access on inline dot key" $ do
    showWithoutTypes (parseExpr ".key {a: 1}") `shouldBe` showWithoutTypes (DictAccess (PDictKey "key") (PDict anyTypeSig [((PDictKey "a"), (PInteger 1))]))

  it "dict access" $ do
    showWithoutTypes (parseExpr "exampledict.key") `shouldBe` showWithoutTypes (DictAccess (PDictKey "key") (Atom anyTypeSig "exampledict"))

  it "dict update" $ do
    showWithoutTypes (parseExpr "{ {a: 1} | a:2 }") `shouldBe` showWithoutTypes (PDictUpdate (PDict anyTypeSig [((PDictKey "a"), (PInteger 1))]) (PDict anyTypeSig [((PDictKey "a"), (PInteger 2))]))

  it "dict update alternative syntax (merge)" $ do
    showWithoutTypes (parseExpr "{ {a: 1} | {a:2} }") `shouldBe` showWithoutTypes (PDictUpdate (PDict anyTypeSig [((PDictKey "a"), (PInteger 1))]) (PDict anyTypeSig [((PDictKey "a"), (PInteger 2))]))

  it "function definiton" $ do
    showWithoutTypes (parseExpr "s x := x * 2") `shouldBe` showWithoutTypes (Binop Assign (Atom anyTypeSig "s") (Lambda anyTypeSig [(Atom anyTypeSig "x")] (Binop Mul (Atom anyTypeSig "x") (PInteger 2))))

  it "nothing" $ do
    showWithoutTypes (parseExpr "Nothing") `shouldBe` showWithoutTypes (PNothing)

  it "just something" $ do
    showWithoutTypes (parseExpr "Just 1") `shouldBe` showWithoutTypes (PJust anyTypeSig (PInteger 1))

  it "internal function" $ do
    showWithoutTypes (parseExpr "(InternalFunction head [xs])") `shouldBe` showWithoutTypes (InternalFunction "head" (PList anyTypeSig [(Atom anyTypeSig "xs")]))

  it "range" $ do
    showWithoutTypes (parseExpr "[1..3]") `shouldBe` showWithoutTypes (PRange anyTypeSig (PInteger 1) (PInteger 3))

  it "range to atom" $ do
    showWithoutTypes (parseExpr "[1..a]") `shouldBe` showWithoutTypes (PRange anyTypeSig (PInteger 1) (Atom anyTypeSig "a"))

  it "tuple" $ do
    showWithoutTypes (parseExpr "{1, a}") `shouldBe` showWithoutTypes (PTuple anyTypeSig [(PInteger 1), (Atom anyTypeSig "a")])

  it "destructuring tuple" $ do
    showWithoutTypes (parseExpr "{a, b} = {1, 2}") `shouldBe` showWithoutTypes (Binop Assign (PTuple anyTypeSig [(Atom anyTypeSig "a"), (Atom anyTypeSig "b")]) (PTuple anyTypeSig [(PInteger 1), (PInteger 2)]))

  describe "Type definition" $ do
    it "Binding definition" $ do
      show (parseExpr "a :: Integer") `shouldBe` show (NamedTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [], typeSigReturn = (IntType)})

    it "Function definition" $ do
      show (parseExpr "a :: Integer -> Integer") `shouldBe` show (NamedTypeSig TypeSig {typeSigName = Just "a", typeSigIn = [IntType], typeSigReturn = (IntType)})

-- xit "Function definition contains function" $ do
--   showWithoutTypes (parseExpr "a :: (Integer -> Integer) -> Integer") `shouldBe` showWithoutTypes (LTypeDef "a" TypeSig {typeSigIn=[Any], typeSigReturn=[Function [Type "Integer", Type "Integer"], Type "Integer"])
