module EvalSpec where

import Data.Map qualified as Map
import Debug.Trace
import Eval
import Lang
import Parser
import ParserSpec
import Syntax
import Test.Hspec

spec :: Spec
spec = describe "Eval" $ do
  describe "Boolean" $ do
    it "negation of bool" $ do
      eval (parseExpr "!true") `shouldBe` BoolVal False

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

  describe "Cmp" $ do
    it "GT" $ do
      eval (parseExpr "1 > 0") `shouldBe` BoolVal True

    it "GTE" $ do
      eval (parseExpr "1 >= 1") `shouldBe` BoolVal True

    it "GTE" $ do
      eval (parseExpr "2 >= 1") `shouldBe` BoolVal True

    it "LT" $ do
      eval (parseExpr "0 < 1") `shouldBe` BoolVal True

    it "LT" $ do
      eval (parseExpr "2 < 1") `shouldBe` BoolVal False

    it "LTE" $ do
      eval (parseExpr "2 <= 1") `shouldBe` BoolVal False

    describe "Lists" $ do
      it "list" $ do
        eval (parseExpr "[1]") `shouldBe` ListVal [IntVal 1]

      it "concatenation" $ do
        eval (parseExpr "[1] ++ [2]") `shouldBe` ListVal [IntVal 1, IntVal 2]

      it "fold function" $ do
        eval (parseExpr "foldInternal (acc x: acc * x) 1 [2, 3]") `shouldBe` IntVal 6

      it "inline partially applied mapping fold function" $ do
        eval (parseExpr "(f: foldInternal (acc x: acc ++ [f x]) [] [1,2]) (x: x*2)") `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "inline fully applied mapping fold function" $ do
        eval (parseExpr "(f xs: foldInternal (acc x: acc ++ [f x]) [] [1,2]) (x: x*2) [1,2]") `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "fold reverse list" $ do
        eval (parseExpr "foldInternal (acc x: [x] ++ acc) [] [1, 2]") `shouldBe` ListVal [IntVal 2, IntVal 1]

      it "let-in binding list" $ do
        eval (parseExpr "let x = [5] in x") `shouldBe` ListVal [IntVal 5]

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
      eval (parseExpr "(x b: x + b) 2") `shouldBe` FunVal (Map.fromList [("x", IntVal 2)], Map.empty) ["x", "b"] (Lambda ["b"] (Binop Add (LInteger 2) (Atom "b")))

    it "lambda fully applied two arguments" $ do
      eval (parseExpr "(x b: x + b) 2 2") `shouldBe` IntVal 4

    it "nested lambda application" $ do
      eval (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` IntVal 8

    it "pipe to pipe" $ do
      eval (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` IntVal 8

    it "nested let-in" $ do
      eval (parseExpr "let k = 1 in (let v = 2 in v + k)") `shouldBe` IntVal 3

    xit "pipes as last argument" $ do
      eval (parseExpr "[1,2] |> map (x: x * 2)") `shouldBe` ListVal [IntVal 2, IntVal 4]

  describe "Stdlib" $ do
    xit "stdlib fold function leveraging foldInternal" $ do
      eval (parseExpr "fold (acc x: acc * x) 1 [2, 3]") `shouldBe` IntVal 6

    it "applied fmap" $ do
      ev <- evalWithLib (parseExpr "map (n: n * 2) [1]")
      ev `shouldBe` ListVal [IntVal 2]

    it "maps over list" $ do
      ev <- evalWithLib (parseExpr "map (x: x * 2) [1,2]")
      ev `shouldBe` ListVal [IntVal 2, IntVal 4]

    it "filters list" $ do
      ev <- evalWithLib (parseExpr "filter (x: x == 2) [1,2]")
      ev `shouldBe` ListVal [IntVal 2]

    it "rejects list" $ do
      ev <- evalWithLib (parseExpr "reject (x: x == 2) [1,2]")
      ev `shouldBe` ListVal [IntVal 1]

    it "list length" $ do
      ev <- evalWithLib (parseExpr "length [1,2]")
      ev `shouldBe` IntVal 2

    it "take" $ do
      ev <- evalWithLib (parseExpr "take 3 [1,2,3,4,5]")
      ev `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

  describe "Multiple expressions" $ do
    it "evals works for one expression" $ do
      evals [parseExpr "1 + 1"] `shouldBe` IntVal 2

    it "keeps env between expressions" $ do
      evals [parseExpr "a = 1", parseExpr "a + 1"] `shouldBe` IntVal 2

    it "bound lambda" $ do
      evals [parseExpr "a = (x: x + 1)", parseExpr "a 1"] `shouldBe` IntVal 2

    it "bound lambda on bound constant" $ do
      evals [parseExpr "f = (x: x + 1)", parseExpr "b = 1", parseExpr "f b"] `shouldBe` IntVal 2

  describe "Dict" $ do
    it "dict" $ do
      eval (parseExpr "{a: 1}") `shouldBe` DictVal (Map.fromList [((DictKeyVal "a"), (IntVal 1))])

    it "dict lookup using _. on atom" $ do
      evals [parseExpr "dict = {a: 1, b: 2}", parseExpr "_.a dict"] `shouldBe` IntVal 1
    it "dict lookup using _. on dict" $ do
      eval (parseExpr "_.a {a: 1, b: 2}") `shouldBe` IntVal 1

    it "dict lookup using inline dict.key" $ do
      eval (parseExpr "{a: 1, b: 2}.a") `shouldBe` IntVal 1

    it "dict lookup using dict.key on atom" $ do
      evals [parseExpr "dict = {a: 1, b: 2}", parseExpr "dict.a"] `shouldBe` IntVal 1

    it "dict update" $ do
      eval (parseExpr "{ {a: 0} | a: 1 }") `shouldBe` DictVal (Map.fromList [((DictKeyVal "a"), (IntVal 1))])

    it "dict update new key" $ do
      eval (parseExpr "{ {a: 0} | b: 1 }") `shouldBe` DictVal (Map.fromList [((DictKeyVal "a"), (IntVal 0)), ((DictKeyVal "b"), (IntVal 1))])

  describe "General" $ do
    it "adds to global scope" $ do
      (val, env) <- evalsWithLib [parseExpr "folder = (f init xs: foldInternal f init xs)", parseExpr "folder (acc x: acc) 1 [1]"]
      Map.keys (fst env) `shouldContain` ["folder"]

    xit "does not leak state" $ do
      (val, env) <- evalsWithLib [parseExpr "fn = (f: f)", parseExpr "fn 1"]
      Map.keys (fst env) `shouldNotContain` ["f"]

    xit "let-in does not leak state" $ do
      (val, env) <- evalsWithLib [parseExpr "let x = 2 in x"]
      Map.keys (fst env) `shouldNotContain` ["x"]

    xit "fold does not leak state" $ do
      (val, env) <- evalsWithLib [parseExpr "foldInternal (acc x: acc) 1 [1]"]
      Map.keys (fst env) `shouldNotContain` ["x"]
      Map.keys (fst env) `shouldNotContain` ["acc"]

    xit "does not leak nested scope (perhaps contrived?)" $ do
      -- evalsWithLib [parseExpr "fn (x: (let b = 1 in b) b)"] `shouldThrow` (== (EvalException ""))
      (val, env) <- evalsWithLib [parseExpr "fn (x: (let b = 1 in b) b)"]
      Map.keys (fst env) `shouldNotContain` ["b"]
      Map.keys (fst env) `shouldNotContain` ["acc"]
