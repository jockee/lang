module ParserSpec where

import Parser
import Syntax
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "true" $ do
    showExpr (parseExpr "true") `shouldBe` showExpr (LBool True)

  it "false" $ do
    showExpr (parseExpr "false") `shouldBe` showExpr (LBool False)

  it "int" $ do
    showExpr (parseExpr "1") `shouldBe` showExpr (LInteger 1)

  it "float" $ do
    showExpr (parseExpr "1.1") `shouldBe` showExpr (LFloat 1.1)

  it "list" $ do
    showExpr (parseExpr "[1,2]") `shouldBe` showExpr (List [LInteger 1, LInteger 2])

  it "handles whitespace" $ do
    showExpr (parseExpr " [ 1 , 2 ] ") `shouldBe` showExpr (List [LInteger 1, LInteger 2])

  it "double equals" $ do
    showExpr (parseExpr "1 == 1") `shouldBe` showExpr (Binop Eql (LInteger 1) (LInteger 1))

  it "not equals" $ do
    showExpr (parseExpr "1 != 1") `shouldBe` showExpr (Binop NotEql (LInteger 1) (LInteger 1))

  it "plus" $ do
    showExpr (parseExpr "1 + 1") `shouldBe` showExpr (Binop Add (LInteger 1) (LInteger 1))

  it "and" $ do
    showExpr (parseExpr "1 && 1") `shouldBe` showExpr (Binop And (LInteger 1) (LInteger 1))

  it "or" $ do
    showExpr (parseExpr "1 || 1") `shouldBe` showExpr (Binop Or (LInteger 1) (LInteger 1))

  it "string" $ do
    showExpr (parseExpr "\"test\"") `shouldBe` showExpr (LString "test")

  it "ternary" $ do
    showExpr (parseExpr "true ? 1 : 2") `shouldBe` showExpr (If (LBool True) (LInteger 1) (LInteger 2))

  it "if-then-else" $ do
    showExpr (parseExpr "if true then 1 else 2") `shouldBe` showExpr (If (LBool True) (LInteger 1) (LInteger 2))

  it "let-in" $ do
    showExpr (parseExpr "let x = 5 in x + 1") `shouldBe` showExpr (App (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))

  it "lambda" $ do
    showExpr (parseExpr "(x: x + 1)") `shouldBe` showExpr (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1)))

  it "pipe to lambda" $ do
    showExpr (parseExpr "5 |> (x: x + 1)") `shouldBe` showExpr (Binop Pipe (LInteger 5) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))))

  it "lambda application" $ do
    showExpr (parseExpr "(x: x + 1) 5") `shouldBe` showExpr (App (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))

  it "pipe to pipe" $ do
    showExpr (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` showExpr (Binop Pipe (Binop Pipe (LInteger 5) (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1)))) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 2))))

  it "pipe partial application" $ do
    showExpr (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` showExpr (Binop Pipe (List [(LInteger 1), (LInteger 2)]) (App (Atom "map") (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2)))))

  it "nested lambda application" $ do
    showExpr (parseExpr "(x: ((y: y + 1) x) + 2) 5") `shouldBe` showExpr (App (Lambda ["x"] (Binop Add (App (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1))) (Atom "x")) (LInteger 2))) (LInteger 5))

  it "nested lambda application 2" $ do
    showExpr (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` showExpr (App (Lambda ["x"] (Binop Add (Atom "x") (App (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1))) (LInteger 2)))) (LInteger 5))

  it "bind function to name in let-in" $ do
    showExpr (parseExpr "let k = (x: x + 1) in k 1") `shouldBe` showExpr (App (Lambda ["k"] (App (Atom "k") (LInteger 1))) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))))

  it "fold function" $ do
    showExpr (parseExpr "foldInternal (acc x: x + 1) 0 [1]") `shouldBe` showExpr (LFold (Lambda ["acc", "x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 0) (List [(LInteger 1)]))

  it "partially applied lambda" $ do
    showExpr (parseExpr "(x y: x + y) 1") `shouldBe` showExpr (App (Lambda ["x", "y"] (Binop Add (Atom "x") (Atom "y"))) (LInteger 1))

  it "multiple argument lambda" $ do
    showExpr (parseExpr "(x y: x + y + 1)") `shouldBe` showExpr (Lambda ["x", "y"] (Binop Add (Binop Add (Atom "x") (Atom "y")) (LInteger 1)))

  it "bind name" $ do
    showExpr (parseExpr "a = 2") `shouldBe` showExpr (Binop Assign (Atom "a") (LInteger 2))

  it "assign list" $ do
    showExpr (parseExpr "xs = [1]") `shouldBe` showExpr (Binop Assign (Atom "xs") (List [(LInteger 1)]))

  it "apply list to lambda" $ do
    showExpr (parseExpr "(s: s) [1]") `shouldBe` showExpr (App (Lambda ["s"] (Atom "s")) (List [(LInteger 1)]))

  it "function application" $ do
    showExpr (parseExpr "(f b: x * b) (x: x*2) a") `shouldBe` showExpr (App (App (Lambda ["f", "b"] (Binop Mul (Atom "x") (Atom "b"))) (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2)))) (Atom "a"))

  it "map expressed as foldInternal" $ do
    showExpr (parseExpr "(f xs: foldInternal (acc x: acc ++ [f x]) [] xs)") `shouldBe` showExpr (Lambda ["f", "xs"] (LFold (Lambda ["acc", "x"] (Binop Concat (Atom "acc") (List [(App (Atom "f") (Atom "x"))]))) (List []) (Atom "xs")))

  it "pass list as function argument" $ do
    showExpr (parseExpr "testFun [1]") `shouldBe` showExpr (App (Atom "testFun") (List [(LInteger 1)]))

  it "map function" $ do
    showExpr (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` showExpr (App (App (Atom "map") (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2)))) (List [(LInteger 1), (LInteger 2)]))

  it "partially applied map" $ do
    showExpr (parseExpr "map (n: n * 2)") `shouldBe` showExpr (App (Atom "map") (Lambda ["n"] (Binop Mul (Atom "n") (LInteger 2))))

  it "list concatenation" $ do
    showExpr (parseExpr "[1] ++ [2]") `shouldBe` showExpr (Binop Concat (List [(LInteger 1)]) (List [(LInteger 2)]))

  it "greater than" $ do
    showExpr (parseExpr "1 > 0") `shouldBe` showExpr (Cmp ">" (LInteger 1) (LInteger 0))

  it "dict" $ do
    showExpr (parseExpr "{a: 1, b: 2}") `shouldBe` showExpr (Dict [((DictKey "a"), (LInteger 1)), ((DictKey "b"), (LInteger 2))])

  it "dict access on atom dot key" $ do
    showExpr (parseExpr ".key exampledict") `shouldBe` showExpr (DictAccess (Atom "key") (Atom "exampledict"))

  it "dict access on inline dot key" $ do
    showExpr (parseExpr ".key {a: 1}") `shouldBe` showExpr (DictAccess (DictKey "key") (Dict [((DictKey "a"), (LInteger 1))]))

  it "dict access" $ do
    showExpr (parseExpr "exampledict.key") `shouldBe` showExpr (DictAccess (DictKey "key") (Atom "exampledict"))

  it "dict update" $ do
    showExpr (parseExpr "{ {a: 1} | a:2 }") `shouldBe` showExpr (DictUpdate (Dict [((DictKey "a"), (LInteger 1))]) (Dict [((DictKey "a"), (LInteger 2))]))

  it "dict update alternative syntax (merge)" $ do
    showExpr (parseExpr "{ {a: 1} | {a:2} }") `shouldBe` showExpr (DictUpdate (Dict [((DictKey "a"), (LInteger 1))]) (Dict [((DictKey "a"), (LInteger 2))]))

  it "function definiton" $ do
    showExpr (parseExpr "s x := x * 2") `shouldBe` showExpr (Binop Assign (Atom "s") (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2))))
