import Control.Exception (evaluate)
import Data.Map qualified as Map
import Eval
import Parser
import Syntax
import Test.Hspec

main :: IO ()
main = test

test :: IO ()
test = hspec $ do
  describe "Eval" $ do
    describe "Boolean" $ do
      it "boolean and true" $ do
        eval (parseExpr "true && true") `shouldBe` BoolVal True

      it "boolean and false" $ do
        eval (parseExpr "true && false") `shouldBe` BoolVal False

      it "boolean or true" $ do
        eval (parseExpr "true || false") `shouldBe` BoolVal True

      it "boolean or false" $ do
        eval (parseExpr "true || false") `shouldBe` BoolVal True

      it "boolean false or false" $ do
        eval (parseExpr "false || false") `shouldBe` BoolVal False

    describe "Equality" $ do
      it "true" $ do
        eval (parseExpr "true") `shouldBe` BoolVal True

      it "integer equality " $ do
        eval (parseExpr "1 == 1") `shouldBe` BoolVal True

      it "integer inequality " $ do
        eval (parseExpr "1 == 2") `shouldBe` BoolVal False

      it "boolean equality" $ do
        eval (parseExpr "true == true") `shouldBe` BoolVal True

      it "boolean inequality" $ do
        eval (parseExpr "true == false") `shouldBe` BoolVal False

      it "mismatching types are unequal" $ do
        eval (parseExpr "true == 1") `shouldBe` BoolVal False

    describe "Arithmetic" $ do
      it "negative integer " $ do
        eval (parseExpr "-1") `shouldBe` IntVal (-1)

      it "integer addition" $ do
        eval (parseExpr "1+1") `shouldBe` IntVal 2

      it "float addition" $ do
        eval (parseExpr "1.0+1.0") `shouldBe` FloatVal 2

      it "integer subtraction" $ do
        eval (parseExpr "12-4") `shouldBe` IntVal 8

      it "float subtraction" $ do
        eval (parseExpr "12.0-4.0") `shouldBe` FloatVal 8.0

      it "integer multiplication" $ do
        eval (parseExpr "2*4") `shouldBe` IntVal 8

      it "float multiplication" $ do
        eval (parseExpr "2.0*4.0") `shouldBe` FloatVal 8.0

    describe "Function application" $ do
      it "let-in with lambda" $ do
        eval (parseExpr "let x = 5 in x + 2") `shouldBe` IntVal 7

      it "lambda one argument" $ do
        eval (parseExpr "(x: x + 1) 2") `shouldBe` IntVal 3

      it "lambda partially applied" $ do
        eval (parseExpr "(x b: x + b) 2") `shouldBe` FunVal (Map.fromList [("x", IntVal 2)]) ["x", "b"] (Lambda ["b"] (Binop Add (LInteger 2) (Atom "b")))

      it "lambda fully applied two arguments" $ do
        eval (parseExpr "(x b: x + b) 2 2") `shouldBe` IntVal 4

      it "nested lambda application" $ do
        eval (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` IntVal 8

      it "pipe to pipe" $ do
        eval (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` IntVal 8

      it "nested let-in" $ do
        eval (parseExpr "let k = 1 in (let v = 2 in v + k)") `shouldBe` IntVal 3

      it "maps over list" $ do
        eval (parseExpr "map (x: x * 2) [1,2]") `shouldBe` ListVal [IntVal 2, IntVal 4]

      xit "pipes as last argument" $ do
        eval (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` ListVal [IntVal 2, IntVal 4]

  describe "Parser" $ do
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

    it "map function" $ do
      showVal (parseExpr "map (x: x * 2) [1, 2]") `shouldBe` showVal (LMap (Lambda ["x"] (Binop Mul (Atom "x") (LInteger 2))) (List [(LInteger 1), (LInteger 2)]))

    it "partially applied lambda" $ do
      showVal (parseExpr "(x y: x + y) 1") `shouldBe` showVal (App (Lambda ["x", "y"] (Binop Add (Atom "x") (Atom "y"))) (LInteger 1))

    it "multiple argument lambda" $ do
      showVal (parseExpr "(x y: x + y + 1)") `shouldBe` showVal (Lambda ["x", "y"] (Binop Add (Binop Add (Atom "x") (Atom "y")) (LInteger 1)))

    xit "partially applied map" $ do
      showVal (parseExpr "map (n: n * 2)") `shouldBe` showVal (LBool True)
