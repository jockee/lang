import Control.Exception (evaluate)
import Eval
import Parser
import Syntax
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Eval" $ do
    describe "Boolean" $ do
      xit "boolean and true" $ do
        eval (parseExpr "true && true") `shouldBe` BoolVal True

      xit "boolean and false" $ do
        eval (parseExpr "true && false") `shouldBe` BoolVal False

      xit "boolean or true" $ do
        eval (parseExpr "true || false") `shouldBe` BoolVal True

      xit "boolean or false" $ do
        eval (parseExpr "true || false") `shouldBe` BoolVal False

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

      xit "lambda two arguments" $ do
        eval (parseExpr "(x b: x + b) 2 2") `shouldBe` IntVal 4

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

    it "if-then-else" $ do
      showVal (parseExpr "if true then 1 else 2") `shouldBe` showVal (If (LBool True) (LInteger 1) (LInteger 2))

    it "let-in" $ do
      showVal (parseExpr "let x = 5 in x + 1") `shouldBe` showVal (App (Abs ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))

    it "lambda" $ do
      showVal (parseExpr "(x: x + 1)") `shouldBe` showVal (Abs ["x"] (Binop Add (Atom "x") (LInteger 1)))

    it "lambda application" $ do
      showVal (parseExpr "(x: x + 1) 5") `shouldBe` showVal (App (Abs ["x"] (Binop Add (Atom "x") (LInteger 1))) (LInteger 5))
