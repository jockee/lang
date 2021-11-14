module ParserSpec where

import Parser
import Syntax
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "true" $ do
    showVal (parseExpr "true") `shouldBe` showVal (LBool True)

  it "false" $ do
    showVal (parseExpr "false") `shouldBe` showVal (LBool False)

  it "int" $ do
    showVal (parseExpr "1") `shouldBe` showVal (LInteger 1)

  it "float" $ do
    showVal (parseExpr "1.1") `shouldBe` showVal (LFloat 1.1)

  it "list" $ do
    showVal (parseExpr "[1,2]") `shouldBe` showVal (List [LInteger 1, LInteger 2])

  it "handles whitespace" $ do
    showVal (parseExpr " [ 1 , 2 ] ") `shouldBe` showVal (List [LInteger 1, LInteger 2])

  it "double equals" $ do
    showVal (parseExpr "1 == 1") `shouldBe` showVal (Binop Eql (LInteger 1) (LInteger 1))

  it "plus" $ do
    showVal (parseExpr "1 + 1") `shouldBe` showVal (Binop Add (LInteger 1) (LInteger 1))

  it "and" $ do
    showVal (parseExpr "1 && 1") `shouldBe` showVal (Binop And (LInteger 1) (LInteger 1))

  it "or" $ do
    showVal (parseExpr "1 || 1") `shouldBe` showVal (Binop Or (LInteger 1) (LInteger 1))

  it "string" $ do
    showVal (parseExpr "\"test\"") `shouldBe` showVal (LString "test")

  it "ternary" $ do
    showVal (parseExpr "true ? 1 : 2") `shouldBe` showVal (If (LBool True) (LInteger 1) (LInteger 2))

  it "if-then-else" $ do
    showVal (parseExpr "if true then 1 else 2") `shouldBe` showVal (If (LBool True) (LInteger 1) (LInteger 2))

  it "let-in" $ do
    showVal (parseExpr "let x = 5 in x + 1") `shouldBe` showVal (App (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))

  it "lambda" $ do
    showVal (parseExpr "(x: x + 1)") `shouldBe` showVal (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1)))

  it "pipe to lambda" $ do
    showVal (parseExpr "5 |> (x: x + 1)") `shouldBe` showVal (Binop Pipe (LInteger 5) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))))

  it "lambda application" $ do
    showVal (parseExpr "(x: x + 1) 5") `shouldBe` showVal (App (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))

  it "pipe to pipe" $ do
    showVal (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` showVal (Binop Pipe (Binop Pipe (LInteger 5) (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1)))) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 2))))

  it "nested lambda application" $ do
    showVal (parseExpr "(x: ((y: y + 1) x) + 2) 5") `shouldBe` showVal (App (Lambda ["x"] (Binop Add (App (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1))) (Atom "x")) (LInteger 2))) (LInteger 5))

  it "nested lambda application 2" $ do
    showVal (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` showVal (App (Lambda ["x"] (Binop Add (Atom "x") (App (Lambda ["y"] (Binop Add (Atom "y") (LInteger 1))) (LInteger 2)))) (LInteger 5))

  it "bind function to name in let-in" $ do
    showVal (parseExpr "let k = (x: x + 1) in k 1") `shouldBe` showVal (App (Lambda ["k"] (App (Atom "k") (LInteger 1))) (Lambda ["x"] (Binop Add (Atom "x") (LInteger 1))))

  it "fold function" $ do
    showVal (parseExpr "foldInternal (acc x: x + 1) 0 [1]") `shouldBe` showVal (LFold (Lambda ["acc", "x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 0) (List [(LInteger 1)]))

  it "map function" $ do
    showVal (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` showVal (LMap (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2))) (List [(LInteger 1), (LInteger 2)]))

  it "partially applied lambda" $ do
    showVal (parseExpr "(x y: x + y) 1") `shouldBe` showVal (App (Lambda ["x", "y"] (Binop Add (Atom "x") (Atom "y"))) (LInteger 1))

  it "multiple argument lambda" $ do
    showVal (parseExpr "(x y: x + y + 1)") `shouldBe` showVal (Lambda ["x", "y"] (Binop Add (Binop Add (Atom "x") (Atom "y")) (LInteger 1)))

  it "bind name" $ do
    showVal (parseExpr "a = 2") `shouldBe` showVal (Binop Assign (Atom "a") (LInteger 2))

  it "assign list" $ do
    showVal (parseExpr "xs = [1]") `shouldBe` showVal (Binop Assign (Atom "xs") (List [(LInteger 1)]))

  it "apply list to lambda" $ do
    showVal (parseExpr "(s: s) [1]") `shouldBe` showVal (App (Lambda ["s"] (Atom "s")) (List [(LInteger 1)]))

  it "function application" $ do
    showVal (parseExpr "(f b: x * b) (x: x*2) a") `shouldBe` showVal (App (App (Lambda ["f", "b"] (Binop Mul (Atom "x") (Atom "b"))) (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2)))) (Atom "a"))

  it "map expressed as foldInternal" $ do
    showVal (parseExpr "(f xs: foldInternal (acc x: acc ++ [f x]) [] xs)") `shouldBe` showVal (Lambda ["f", "xs"] (LFold (Lambda ["acc", "x"] (LConcat (Atom "acc") (List [(App (Atom "f") (Atom "x"))]))) (List []) (Atom "xs")))

  xit "pass list as function argument" $ do
    showVal (parseExpr "testFun [1]") `shouldBe` showVal (App (Atom "testFun") (List [(LInteger 1)]))

  xit "partially applied map" $ do
    showVal (parseExpr "map (n: n * 2)") `shouldBe` showVal (LBool True)

  it "concatenation" $ do
    showVal (parseExpr "[1] ++ [2]") `shouldBe` showVal (LConcat (List [(LInteger 1)]) (List [(LInteger 2)]))
