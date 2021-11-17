module ParserSpec where

import Parser
import Syntax
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "true" $ do
    showExpr (parseExpr "true") `shouldBe` showExpr (PBool True)

  it "false" $ do
    showExpr (parseExpr "false") `shouldBe` showExpr (PBool False)

  it "int" $ do
    showExpr (parseExpr "1") `shouldBe` showExpr (PInteger 1)

  it "float" $ do
    showExpr (parseExpr "1.1") `shouldBe` showExpr (PFloat 1.1)

  it "list" $ do
    showExpr (parseExpr "[1,2]") `shouldBe` showExpr (PList [PInteger 1, PInteger 2])

  it "handles whitespace" $ do
    showExpr (parseExpr " [ 1 , 2 ] ") `shouldBe` showExpr (PList [PInteger 1, PInteger 2])

  it "double equals" $ do
    showExpr (parseExpr "1 == 1") `shouldBe` showExpr (Binop Eql (PInteger 1) (PInteger 1))

  it "not equals" $ do
    showExpr (parseExpr "1 != 1") `shouldBe` showExpr (Binop NotEql (PInteger 1) (PInteger 1))

  it "plus" $ do
    showExpr (parseExpr "1 + 1") `shouldBe` showExpr (Binop Add (PInteger 1) (PInteger 1))

  it "and" $ do
    showExpr (parseExpr "1 && 1") `shouldBe` showExpr (Binop And (PInteger 1) (PInteger 1))

  it "or" $ do
    showExpr (parseExpr "1 || 1") `shouldBe` showExpr (Binop Or (PInteger 1) (PInteger 1))

  it "string" $ do
    showExpr (parseExpr "\"test\"") `shouldBe` showExpr (PString "test")

  it "ternary" $ do
    showExpr (parseExpr "true ? 1 : 2") `shouldBe` showExpr (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "if-then-else" $ do
    showExpr (parseExpr "if true then 1 else 2") `shouldBe` showExpr (PIf (PBool True) (PInteger 1) (PInteger 2))

  it "let-in" $ do
    showExpr (parseExpr "let x = 5 in x + 1") `shouldBe` showExpr (App (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 1))) (PInteger 5))

  it "lambda" $ do
    showExpr (parseExpr "(x: x + 1)") `shouldBe` showExpr (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 1)))

  it "pipe to lambda" $ do
    showExpr (parseExpr "5 |> (x: x + 1)") `shouldBe` showExpr (Binop Pipe (PInteger 5) (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 1))))

  it "lambda application" $ do
    showExpr (parseExpr "(x: x + 1) 5") `shouldBe` showExpr (App (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 1))) (PInteger 5))

  it "pipe to pipe" $ do
    showExpr (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` showExpr (Binop Pipe (Binop Pipe (PInteger 5) (Lambda [(Atom "y")] (Binop Add (Atom "y") (PInteger 1)))) (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 2))))

  it "pipe partial application" $ do
    showExpr (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` showExpr (Binop Pipe (PList [(PInteger 1), (PInteger 2)]) (App (Atom "map") (Lambda [(Atom "x")] (Binop Mul (Atom "x") (PInteger 2)))))

  it "nested lambda application" $ do
    showExpr (parseExpr "(x: ((y: y + 1) x) + 2) 5") `shouldBe` showExpr (App (Lambda [(Atom "x")] (Binop Add (App (Lambda [(Atom "y")] (Binop Add (Atom "y") (PInteger 1))) (Atom "x")) (PInteger 2))) (PInteger 5))

  it "nested lambda application 2" $ do
    showExpr (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` showExpr (App (Lambda [(Atom "x")] (Binop Add (Atom "x") (App (Lambda [(Atom "y")] (Binop Add (Atom "y") (PInteger 1))) (PInteger 2)))) (PInteger 5))

  it "bind function to name in let-in" $ do
    showExpr (parseExpr "let k = (x: x + 1) in k 1") `shouldBe` showExpr (App (Lambda [(Atom "k")] (App (Atom "k") (PInteger 1))) (Lambda [(Atom "x")] (Binop Add (Atom "x") (PInteger 1))))

  it "partially applied lambda" $ do
    showExpr (parseExpr "(x y: x + y) 1") `shouldBe` showExpr (App (Lambda [(Atom "x"), (Atom "y")] (Binop Add (Atom "x") (Atom "y"))) (PInteger 1))

  it "multiple argument lambda" $ do
    showExpr (parseExpr "(x y: x + y + 1)") `shouldBe` showExpr (Lambda [(Atom "x"), (Atom "y")] (Binop Add (Binop Add (Atom "x") (Atom "y")) (PInteger 1)))

  it "bind name" $ do
    showExpr (parseExpr "a = 2") `shouldBe` showExpr (Binop Assign (Atom "a") (PInteger 2))

  it "assign list" $ do
    showExpr (parseExpr "xs = [1]") `shouldBe` showExpr (Binop Assign (Atom "xs") (PList [(PInteger 1)]))

  it "apply list to lambda" $ do
    showExpr (parseExpr "(s: s) [1]") `shouldBe` showExpr (App (Lambda [(Atom "s")] (Atom "s")) (PList [(PInteger 1)]))

  it "function application" $ do
    showExpr (parseExpr "(f b: x * b) (x: x*2) a") `shouldBe` showExpr (App (App (Lambda [(Atom "f"), (Atom "b")] (Binop Mul (Atom "x") (Atom "b"))) (Lambda [(Atom "x")] (Binop Mul (Atom "x") (PInteger 2)))) (Atom "a"))

  it "pass list as function argument" $ do
    showExpr (parseExpr "testFun [1]") `shouldBe` showExpr (App (Atom "testFun") (PList [(PInteger 1)]))

  it "map function" $ do
    showExpr (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` showExpr (App (App (Atom "map") (Lambda [(Atom "x")] (Binop Mul (Atom "x") (PInteger 2)))) (PList [(PInteger 1), (PInteger 2)]))

  it "partially applied map" $ do
    showExpr (parseExpr "map (n: n * 2)") `shouldBe` showExpr (App (Atom "map") (Lambda [(Atom "n")] (Binop Mul (Atom "n") (PInteger 2))))

  it "list concatenation" $ do
    showExpr (parseExpr "[1] ++ [2]") `shouldBe` showExpr (Binop Concat (PList [(PInteger 1)]) (PList [(PInteger 2)]))

  it "greater than" $ do
    showExpr (parseExpr "1 > 0") `shouldBe` showExpr (Cmp ">" (PInteger 1) (PInteger 0))

  it "dict" $ do
    showExpr (parseExpr "{a: 1, b: 2}") `shouldBe` showExpr (PDict [((PDictKey "a"), (PInteger 1)), ((PDictKey "b"), (PInteger 2))])

  it "empty dict" $ do
    showExpr (parseExpr "{}") `shouldBe` showExpr (PDict [])

  it "dict access on atom dot key" $ do
    showExpr (parseExpr ".key exampledict") `shouldBe` showExpr (DictAccess (Atom "key") (Atom "exampledict"))

  it "dict access on inline dot key" $ do
    showExpr (parseExpr ".key {a: 1}") `shouldBe` showExpr (DictAccess (PDictKey "key") (PDict [((PDictKey "a"), (PInteger 1))]))

  it "dict access" $ do
    showExpr (parseExpr "exampledict.key") `shouldBe` showExpr (DictAccess (PDictKey "key") (Atom "exampledict"))

  it "dict update" $ do
    showExpr (parseExpr "{ {a: 1} | a:2 }") `shouldBe` showExpr (PDictUpdate (PDict [((PDictKey "a"), (PInteger 1))]) (PDict [((PDictKey "a"), (PInteger 2))]))

  it "dict update alternative syntax (merge)" $ do
    showExpr (parseExpr "{ {a: 1} | {a:2} }") `shouldBe` showExpr (PDictUpdate (PDict [((PDictKey "a"), (PInteger 1))]) (PDict [((PDictKey "a"), (PInteger 2))]))

  it "function definiton" $ do
    showExpr (parseExpr "s x := x * 2") `shouldBe` showExpr (Binop Assign (Atom "s") (Lambda [(Atom "x")] (Binop Mul (Atom "x") (PInteger 2))))

  it "nothing" $ do
    showExpr (parseExpr "Nothing") `shouldBe` showExpr (PNothing)

  it "just something" $ do
    showExpr (parseExpr "Just 1") `shouldBe` showExpr (PJust (PInteger 1))

  it "internal function" $ do
    showExpr (parseExpr "(InternalFunction head [xs])") `shouldBe` showExpr (InternalFunction "head" (PList [(Atom "xs")]))

  it "range" $ do
    showExpr (parseExpr "[1..3]") `shouldBe` showExpr (PRange (PInteger 1) (PInteger 3))

  it "range to atom" $ do
    showExpr (parseExpr "[1..a]") `shouldBe` showExpr (PRange (PInteger 1) (Atom "a"))

  it "tuple" $ do
    showExpr (parseExpr "{1, a}") `shouldBe` showExpr (PTuple [(PInteger 1), (Atom "a")])

  it "destructuring tuple" $ do
    showExpr (parseExpr "{a, b} = {1, 2}") `shouldBe` showExpr (Binop Assign (PTuple [(Atom "a"), (Atom "b")]) (PTuple [(PInteger 1), (PInteger 2)]))
