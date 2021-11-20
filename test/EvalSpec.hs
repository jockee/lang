module EvalSpec where

import Control.Exception
import Data.Map qualified as Map
import Debug.Trace
import Eval
import Exceptions
import Lang
import Parser
import Syntax
import Test.Hspec
import TypeCheck

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
      eval (parseExpr "(x b: x + b) 2")
        `shouldSatisfy` ( \case
                            FunctionVal {} -> True
                            _ -> False
                        )

    it "lambda fully applied two arguments" $ do
      eval (parseExpr "(x b: x + b) 2 2") `shouldBe` IntVal 4

    it "nested lambda application" $ do
      eval (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` IntVal 8

    it "pipe to pipe" $ do
      eval (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` IntVal 8

    it "nested let-in" $ do
      eval (parseExpr "let k = 1 in (let v = 2 in v + k)") `shouldBe` IntVal 3

    it "pipes as last argument" $ do
      eval (parseExpr "[1,2] |> (x: x ++ [3])") `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

  describe "Stdlib" $ do
    it "fold function" $ do
      ev <- evalWithLib (parseExpr "fold (acc x: acc * x) 1 [2, 3]")
      ev `shouldBe` IntVal 6

    it "inline partially applied mapping fold function" $ do
      ev <- evalWithLib (parseExpr "(f: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2)")
      ev `shouldBe` ListVal [IntVal 2, IntVal 4]

    it "inline fully applied mapping fold function" $ do
      ev <- evalWithLib (parseExpr "(f xs: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2) [1,2]")
      ev `shouldBe` ListVal [IntVal 2, IntVal 4]

    it "reverse" $ do
      ev <- evalWithLib (parseExpr "reverse [1, 2]")
      ev `shouldBe` ListVal [IntVal 2, IntVal 1]

    it "max" $ do
      ev <- evalWithLib (parseExpr "max [1, 2]")
      ev `shouldBe` JustVal (IntVal 2)

    it "head" $ do
      ev <- evalWithLib (parseExpr "head [1]")
      ev `shouldBe` JustVal (IntVal 1)

    it "stdlib fold function leveraging foldInternal" $ do
      ev <- evalWithLib (parseExpr "fold (acc x: acc * x) 1 [2, 3]")
      ev `shouldBe` IntVal 6

    it "applied map" $ do
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

    it "toList" $ do
      ev <- evalWithLib (parseExpr "toList {a: 1, b: 2}")
      ev `shouldBe` ListVal [(TupleVal [DictKey "a", IntVal 1]), (TupleVal [DictKey "b", IntVal 2])]

    it "values" $ do
      ev <- evalWithLib (parseExpr "values {a: 1, b: 2}")
      ev `shouldBe` ListVal [IntVal 1, IntVal 2]

    it "keys" $ do
      ev <- evalWithLib (parseExpr "keys {a: 1, b: 2}")
      ev `shouldBe` ListVal [DictKey "a", DictKey "b"]

    it "merge" $ do
      ev <- evalWithLib (parseExpr "merge {a: 1} {b: 2}")
      ev `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1), (DictKey "b", IntVal 2)])

    it "toDict" $ do
      ev <- evalWithLib (parseExpr "toDict [{\"a\", 1}]")
      ev `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1)])

  describe "Multiple expressions" $ do
    it "evals works for one expression" $ do
      evals [parseExpr "1 + 1"] `shouldBe` IntVal 2

    it "keeps env between expressions" $ do
      evals [parseExpr "a = 1", parseExpr "a + 1"] `shouldBe` IntVal 2

    it "bound lambda" $ do
      evals [parseExpr "a = (x: x + 1)", parseExpr "a 1"] `shouldBe` IntVal 2

    it "bound lambda on bound constant" $ do
      evals [parseExpr "f = (x: x + 1)", parseExpr "b = 1", parseExpr "f b"] `shouldBe` IntVal 2

    it "evals works for one expression" $ do
      evals [parseExpr "s x := x * 2", parseExpr "s 2"] `shouldBe` IntVal 4

  describe "Dict" $ do
    it "dict" $ do
      eval (parseExpr "{a: 1}") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1))])

    it "dict lookup using . on atom" $ do
      evals [parseExpr "dict = {a: 1, b: 2}", parseExpr ".a dict"] `shouldBe` IntVal 1
    it "dict lookup using . on dict" $ do
      eval (parseExpr ".a {a: 1, b: 2}") `shouldBe` IntVal 1

    it "dict lookup using dict.key on atom" $ do
      evals [parseExpr "dict = {a: 1, b: 2}", parseExpr "dict.a"] `shouldBe` IntVal 1

    it "dict update" $ do
      eval (parseExpr "{ {a: 0} | a: 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1))])

    it "dict update new key" $ do
      eval (parseExpr "{ {a: 0} | b: 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 0)), ((DictKey "b"), (IntVal 1))])

    it "dict update on atom" $ do
      evals [parseExpr "dict = {b: 2}", parseExpr "{ dict | b: 1 }"] `shouldBe` DictVal (Map.fromList [((DictKey "b"), (IntVal 1))])

  it "dict dynamic key" $
    evals (parseExprs "a = \"s\"; { a => 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "s"), (IntVal 1))])

  it "dict update dynamic key" $
    evals (parseExprs "a = \"s\"; { {a: 1} | a => 2 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1)), ((DictKey "s"), (IntVal 2))])

  describe "General" $ do
    it "adds to global scope" $ do
      (val, env) <- evalsWithLib $ parseExprs "folder = 1"
      Map.keys (envValues env) `shouldContain` ["global:folder"]

    it "assignment in lambda does not leak" $ do
      (val, env) <- evalsWithLib $ parseExprs "fn = (x: f = 1); fn 1"
      Map.keys (envValues env) `shouldNotContain` ["global:f"]

    it "moves back up to global" $ do
      (val, env) <- evalsWithLib $ parseExprs "fn = (f: f); fn 1; a = 1"
      Map.keys (envValues env) `shouldContain` ["global:a"]

    it "does not leak state" $ do
      (val, env) <- evalsWithLib $ parseExprs "fn = (f: f); fn 1; a = 1"
      Map.keys (envValues env) `shouldNotContain` ["global:f"]

    it "let-in does not leak state" $ do
      (val, env) <- evalsWithLib $ parseExprs "let x = 2 in x"
      Map.keys (envValues env) `shouldNotContain` ["global:x"]

    it "fold does not leak state" $ do
      (val, env) <- evalsWithLib $ parseExprs "fold (acc x: acc) 1 [1]"
      Map.keys (envValues env) `shouldNotContain` ["global:x"]
      Map.keys (envValues env) `shouldNotContain` ["global:acc"]

    it "does not leak nested scope" $ do
      evaluate (evals (parseExprs "fn (x: (let b = 1 in b) b)")) `shouldThrow` anyException

    it "multiple assignments" $ do
      evals (parseExprs "a [] := 1; a b := 2; a []") `shouldBe` IntVal 1

  describe "Tuple" $ do
    it "destructuring tuple returns itself" $ do
      eval (parseExpr "{a, b} = {1, 2}") `shouldBe` (TupleVal [IntVal 1, IntVal 2])

    it "destructuring tuple pushes to scope" $ do
      evals (parseExprs "{a, b} = {1, 2}; a + b") `shouldBe` IntVal 3

    it "destructuring nested tuple" $ do
      eval (parseExpr "{a, {b, c}} = {1, {2, 3}}") `shouldBe` (TupleVal [IntVal 1, (TupleVal [IntVal 2, IntVal 3])])

    it "destructuring nested tuple pushes to scope" $ do
      evals (parseExprs "{a, {b, c}} = {1, {2, 3}}; a + b + c") `shouldBe` IntVal 6

    it "underscore ignores" $ do
      evals (parseExprs "{_, b} = {1, 2}; b") `shouldBe` IntVal 2

    it "underscore ignores" $ do
      evaluate (evals (parseExprs "{_, b} = {1, 2}; _")) `shouldThrow` anyException

    it "destructuring tuple too many on left side fails" $ do
      evaluate (eval (parseExpr "{a, b} = {1}")) `shouldThrow` anyException

    it "destructuring tuple too many on right side fails" $ do
      evaluate (eval (parseExpr "{a, b} = {1, 2, 3}")) `shouldThrow` anyException

    it "destructuring tuple with atom on right side" $ do
      evals (parseExprs "c = 1; {a, b} = {1, c}") `shouldBe` (TupleVal [IntVal 1, IntVal 1])

    it "destructuring tuple with expression on right side" $ do
      evals (parseExprs "{a, b} = {1, (c = 1)}") `shouldBe` (TupleVal [IntVal 1, IntVal 1])

    it "destructuring in lambda (one arg)" $ do
      evals (parseExprs "({a}: a + 1) {1}") `shouldBe` IntVal 2

    it "destructuring in lambda (two args)" $ do
      evals (parseExprs "({a,b}: a + b) {1,2}") `shouldBe` IntVal 3

    it "destructuring in lambda (four args)" $ do
      evals (parseExprs "({a,b,c,d}: a + b + c  + d) {1,2,3,4}") `shouldBe` IntVal 10

    it "destructuring in lambda (nested)" $ do
      evals (parseExprs "({a,{b,c}}: a + b + c) {1,{2,3}}") `shouldBe` IntVal 6

  describe "Range" $ do
    it "range" $ do
      eval (parseExpr "[1..3]") `shouldBe` (ListVal [IntVal 1, IntVal 2, IntVal 3])

    it "range on atom" $ do
      evals (parseExprs "a = 3; [1..a]") `shouldBe` (ListVal [IntVal 1, IntVal 2, IntVal 3])

  describe "Internal functions" $ do
    it "head" $ do
      eval (parseExpr "(InternalFunction head [2, 3])") `shouldBe` JustVal (IntVal 2)

    it "head returns Nothing on empty list" $ do
      eval (parseExpr "(InternalFunction head [])") `shouldBe` NothingVal

    it "sort" $ do
      eval (parseExpr "(InternalFunction sort [3, 2])") `shouldBe` ListVal [IntVal 2, IntVal 3]

    it "zipWith" $ do
      eval (parseExpr "(InternalFunction zipWith [(x y: [x, y]), [1,2, 3], [3, 2]])") `shouldBe` ListVal [ListVal [IntVal 1, IntVal 3], ListVal [IntVal 2, IntVal 2]]

  describe "Runtime type system" $ do
    xit "Can't declare Integer as String" $ do
      evaluate (eval (parseExpr "i = 1 :: String")) `shouldThrow` anyException

    xit "Can't declare integer as string typedef" $ do
      evaluate (evals (parseExprs "a :: String; a = 1")) `shouldThrow` anyException

    xit "Too many arguments in function definition" $ do
      evaluate (evals (parseExprs "a :: Integer -> Integer; a b c = b + c ")) `shouldThrow` anyException

    it "Binding definition pushed to env" $ do
      evalIn emptyEnv (parseExpr "a :: Integer")
        `shouldSatisfy` ( \case
                            (_, env) ->
                              Map.lookup "a" (typeSigs env)
                                == Just (TypeSig {typeSigName = Just "a", typeSigIn = [], typeSigReturn = IntType})
                        )

    it "Called with wrong type" $ do
      evaluate (evals (parseExprs "a :: Integer -> Integer; a b := b + 1; a \"s\"")) `shouldThrow` anyException

    it "Called with wrong type second argument" $ do
      evaluate (evals (parseExprs "a :: Integer -> Integer -> Integer; a b c := b + 1; a 1 \"s\"")) `shouldThrow` anyException

    it "Correct types, two different" $ do
      evals (parseExprs "a :: Integer -> String -> Integer; a b c := b + 1; a 1 \"s\"") `shouldBe` IntVal 2

    it "Called with wrong type second argument, different types" $ do
      evaluate (evals (parseExprs "a :: String -> Integer -> Integer; a b c := c + 1; a 1 \"s\"")) `shouldThrow` anyException

  describe "Pattern matching" $ do
    it "can fall through" $ do
      evals (parseExprs "a [] := 1; a b := [2]; a 3") `shouldBe` ListVal [IntVal 2]

    it "empty list should only match empty list" $ do
      evals (parseExprs "a [] := [0]; a b := [2]; a [1]") `shouldBe` ListVal [IntVal 2]

    it "matches specific integer value" $ do
      evals (parseExprs "f 1 := 2; f s := 3; f 1") `shouldBe` IntVal 2

    it "falls through non-matching integer value" $ do
      evals (parseExprs "f 1 := 2; f s := 3; f 2") `shouldBe` IntVal 3

  describe "Modules" $ do
    it "evals module" $
      evals (parseExprs "module A { 1 }") `shouldBe` IntVal 1

    xit "can't call without namespacing outside of module" $
      evaluate (evals (parseExprs "module A { s = 1 }; s")) `shouldThrow` anyException

    it "can call with namespacing outside of module" $
      evals (parseExprs "module A { s = 1 }; A.s") `shouldBe` IntVal 1

    it "can call without namespacing inside module" $
      evals (parseExprs "module A { s = 1; s }") `shouldBe` IntVal 1
