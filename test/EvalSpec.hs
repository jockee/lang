{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module EvalSpec where

import Control.Exception
import Data.Map qualified as Map
import Data.String.Interpolate (i, iii)
import Debug.Trace
import Eval
import Exceptions
import Lang
import Parser
import Syntax
import Test.Hspec
import TypeCheck

type TestEnv = [Expr]

spec :: Spec
spec = beforeAll (let !std = evaledStdLibEnv in std) $
  describe "Eval" $ do
    describe "Boolean" $ do
      it "negation of bool" $ \stdLibEnv ->
        eval (parseExpr "!true") `shouldBe` BoolVal False

      it "boolean and true" $ \stdLibEnv ->
        eval (parseExpr "true && true") `shouldBe` BoolVal True

      it "boolean and false" $ \stdLibEnv ->
        eval (parseExpr "true && false") `shouldBe` BoolVal False

      it "boolean or true" $ \stdLibEnv ->
        eval (parseExpr "true || false") `shouldBe` BoolVal True

      it "boolean or false" $ \stdLibEnv ->
        eval (parseExpr "true || false") `shouldBe` BoolVal True

      it "boolean false or false" $ \stdLibEnv ->
        eval (parseExpr "false || false") `shouldBe` BoolVal False

    describe "Equality" $ do
      it "true" $ \stdLibEnv ->
        eval (parseExpr "true") `shouldBe` BoolVal True

      it "integer equality " $ \stdLibEnv ->
        eval (parseExpr "1 == 1") `shouldBe` BoolVal True

      it "integer inequality " $ \stdLibEnv ->
        eval (parseExpr "1 == 2") `shouldBe` BoolVal False

      it "boolean equality" $ \stdLibEnv ->
        eval (parseExpr "true == true") `shouldBe` BoolVal True

      it "boolean inequality" $ \stdLibEnv ->
        eval (parseExpr "true == false") `shouldBe` BoolVal False

      it "mismatching types are unequal" $ \stdLibEnv ->
        eval (parseExpr "true == 1") `shouldBe` BoolVal False

    describe "Cmp" $ do
      it "GT" $ \stdLibEnv ->
        eval (parseExpr "1 > 0") `shouldBe` BoolVal True

      it "GTE" $ \stdLibEnv ->
        eval (parseExpr "1 >= 1") `shouldBe` BoolVal True

      it "GTE" $ \stdLibEnv ->
        eval (parseExpr "2 >= 1") `shouldBe` BoolVal True

      it "LT" $ \stdLibEnv ->
        eval (parseExpr "0 < 1") `shouldBe` BoolVal True

      it "LT" $ \stdLibEnv ->
        eval (parseExpr "2 < 1") `shouldBe` BoolVal False

      it "LTE" $ \stdLibEnv ->
        eval (parseExpr "2 <= 1") `shouldBe` BoolVal False

      describe "Lists" $ do
        it "list" $ \stdLibEnv ->
          eval (parseExpr "[1]") `shouldBe` ListVal [IntVal 1]

        it "concatenation" $ \stdLibEnv ->
          eval (parseExpr "[1] ++ [2]") `shouldBe` ListVal [IntVal 1, IntVal 2]

        it "let-in binding list" $ \stdLibEnv ->
          eval (parseExpr "let x = [5]: x") `shouldBe` ListVal [IntVal 5]

        it "let-in binding list" $ \stdLibEnv ->
          eval (parseExpr "let (a, b) = (1,2): a+b") `shouldBe` IntVal 3

        it "let-in binding list multiple bindings" $ \stdLibEnv ->
          eval (parseExpr "let x = 5, y = 2: x + y") `shouldBe` IntVal 7

        it "let-in binding list multiple bindings with dependency" $ \stdLibEnv ->
          eval (parseExpr "let x = 5, y = (x + 2): x + y") `shouldBe` IntVal 12

    describe "Arithmetic" $ do
      it "modulo" $ \stdLibEnv ->
        eval (parseExpr "3%2") `shouldBe` IntVal 1

      it "toFloat on integer" $ \stdLibEnv ->
        eval (parseExpr "toFloat 3") `shouldBe` FloatVal 3

      it "toFloat on float" $ \stdLibEnv ->
        eval (parseExpr "toFloat 3.0") `shouldBe` FloatVal 3

      it "toInteger on float " $ \stdLibEnv ->
        eval (parseExpr "toInteger 3.0") `shouldBe` IntVal 3

      it "toInteger on integer" $ \stdLibEnv ->
        eval (parseExpr "toInteger 3") `shouldBe` IntVal 3

      it "subtraction" $ \stdLibEnv ->
        eval (parseExpr "1-1") `shouldBe` IntVal 0

      it "subtraction" $ \stdLibEnv ->
        evals (parseExprs "a = 1; (a-1)") `shouldBe` IntVal 0

      it "negative integer " $ \stdLibEnv ->
        eval (parseExpr "-1") `shouldBe` IntVal (-1)

      it "integer addition" $ \stdLibEnv ->
        eval (parseExpr "1+1") `shouldBe` IntVal 2

      it "float addition" $ \stdLibEnv ->
        eval (parseExpr "1.0+1.0") `shouldBe` FloatVal 2

      it "integer subtraction" $ \stdLibEnv ->
        eval (parseExpr "12-4") `shouldBe` IntVal 8

      it "float subtraction" $ \stdLibEnv ->
        eval (parseExpr "12.0-4.0") `shouldBe` FloatVal 8.0

      it "integer multiplication" $ \stdLibEnv ->
        eval (parseExpr "2*4") `shouldBe` IntVal 8

      it "float multiplication" $ \stdLibEnv ->
        eval (parseExpr "2.0*4.0") `shouldBe` FloatVal 8.0

    describe "Function application" $ do
      it "let-in with lambda" $ \stdLibEnv ->
        eval (parseExpr "let x = 5: x + 2") `shouldBe` IntVal 7

      it "lambda one argument" $ \stdLibEnv ->
        eval (parseExpr "(x: x + 1) 2") `shouldBe` IntVal 3

      it "lambda partially applied" $ \stdLibEnv ->
        eval (parseExpr "(x b: x + b) 2")
          `shouldSatisfy` ( \case
                              FunctionVal {} -> True
                              _ -> False
                          )

      it "lambda fully applied two arguments" $ \stdLibEnv ->
        eval (parseExpr "(x b: x + b) 2 2") `shouldBe` IntVal 4

      it "nested lambda application" $ \stdLibEnv ->
        eval (parseExpr "(x: x + (y: y + 1) 2) 5") `shouldBe` IntVal 8

      it "pipe to pipe" $ \stdLibEnv ->
        eval (parseExpr "5 |> (y: y + 1) |> (x: x + 2)") `shouldBe` IntVal 8

      it "nested let-in" $ \stdLibEnv ->
        eval (parseExpr "let k = 1: (let v = 2: v + k)") `shouldBe` IntVal 3

      it "pipes as last argument" $ \stdLibEnv ->
        eval (parseExpr "[1,2] |> (x: x ++ [3])") `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

    describe "Stdlib" $ do
      it "fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "fold (acc x: acc * x) 1 [2, 3]")
        val `shouldBe` IntVal 6

      it "inline partially applied mapping fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "(f: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2)")
        val `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "inline fully applied mapping fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "(f xs: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2) [1,2]")
        val `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "reverse" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "reverse [1, 2]")
        val `shouldBe` ListVal [IntVal 2, IntVal 1]

      it "max" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "max [1, 2]")
        val `shouldBe` DataVal "Maybe" "Some" [IntVal 2]

      it "head" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "head [1]")
        val `shouldBe` DataVal "Maybe" "Some" [IntVal 1]

      it "stdlib fold function leveraging foldInternal" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "fold (acc x: acc * x) 1 [2, 3]")
        val `shouldBe` IntVal 6

      it "applied map" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "map (n: n * 2) [1]")
        val `shouldBe` ListVal [IntVal 2]

      it "maps over list" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "map (x: x * 2) [1,2]")
        val `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "filters list" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "filter (x: x == 2) [1,2]")
        val `shouldBe` ListVal [IntVal 2]

      it "rejects list" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "reject (x: x == 2) [1,2]")
        val `shouldBe` ListVal [IntVal 1]

      it "list length" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "length [1,2]")
        val `shouldBe` IntVal 2

      it "take" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "take 3 [1,2,3,4,5]")
        val `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

      it "toList" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "toList {a: 1, b: 2}")
        val `shouldBe` ListVal [TupleVal [DictKey "a", IntVal 1], TupleVal [DictKey "b", IntVal 2]]

      it "values" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "values {a: 1, b: 2}")
        val `shouldBe` ListVal [IntVal 1, IntVal 2]

      it "keys" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "keys {a: 1, b: 2}")
        val `shouldBe` ListVal [DictKey "a", DictKey "b"]

      it "merge" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "merge {a: 1} {b: 2}")
        val `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1), (DictKey "b", IntVal 2)])

      it "toDict" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "toDict [(\"a\", 1)]")
        val `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1)])

      describe "Multiple expressions" $ do
        it "evals works for one expression" $ \stdLibEnv ->
          evals [parseExpr "1 + 1"] `shouldBe` IntVal 2

        it "keeps env between expressions" $ \stdLibEnv ->
          evals [parseExpr "a = 1", parseExpr "a + 1"] `shouldBe` IntVal 2

        it "bound lambda" $ \stdLibEnv ->
          evals [parseExpr "a = (x: x + 1)", parseExpr "a 1"] `shouldBe` IntVal 2

        it "bound lambda on bound constant" $ \stdLibEnv ->
          evals [parseExpr "f = (x: x + 1)", parseExpr "b = 1", parseExpr "f b"] `shouldBe` IntVal 2

        it "evals works for one expression" $ \stdLibEnv ->
          evals [parseExpr "s x = x * 2", parseExpr "s 2"] `shouldBe` IntVal 4

      describe "Dict" $ do
        it "dict" $ \stdLibEnv ->
          eval (parseExpr "{a: 1}") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1))])

        it "dict lookup using . on atom" $ \stdLibEnv ->
          evals [parseExpr "dict = {a: 1, b: 2}", parseExpr ".a dict"] `shouldBe` IntVal 1
        it "dict lookup using . on dict" $ \stdLibEnv ->
          eval (parseExpr ".a {a: 1, b: 2}") `shouldBe` IntVal 1

        it "dict lookup using dict.key on atom" $ \stdLibEnv ->
          evals [parseExpr "dict = {a: 1, b: 2}", parseExpr "dict.a"] `shouldBe` IntVal 1

        it "dict update" $ \stdLibEnv ->
          eval (parseExpr "{ {a: 0} | a: 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1))])

        it "dict update new key" $ \stdLibEnv ->
          eval (parseExpr "{ {a: 0} | b: 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 0)), ((DictKey "b"), (IntVal 1))])

        it "dict update on atom" $ \stdLibEnv ->
          evals [parseExpr "dict = {b: 2}", parseExpr "{ dict | b: 1 }"] `shouldBe` DictVal (Map.fromList [((DictKey "b"), (IntVal 1))])

      it "dict dynamic key" $ \stdLibEnv ->
        evals (parseExprs "a = \"s\"; { a => 1 }") `shouldBe` DictVal (Map.fromList [((DictKey "s"), (IntVal 1))])

      it "dict update dynamic key" $ \stdLibEnv ->
        evals (parseExprs "a = \"s\"; { {a: 1} | a => 2 }") `shouldBe` DictVal (Map.fromList [((DictKey "a"), (IntVal 1)), ((DictKey "s"), (IntVal 2))])

      it "destructuring dict basic" $ \stdLibEnv ->
        evals (parseExprs "let {a: b} = {a: 2}: b") `shouldBe` IntVal 2

      it "destructuring dict, requiring matching other values - succeeding" $ \stdLibEnv ->
        evals (parseExprs "let {a: b, c: 1} = {a: 2, c: 1}: b") `shouldBe` IntVal 2

      it "destructuring dict, requiring matching other values - failing" $ \stdLibEnv ->
        evaluate (evals (parseExprs "let {a: b, c: 1} = {a: 2, c: 2}: b")) `shouldThrow` anyException

      describe "General" $ do
        it "adds to global scope" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "folder = 1"
          Map.keys (envValues env) `shouldContain` ["global:folder"]

        it "assignment in lambda does not leak" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "fn = (x: f = 1); fn 1"
          Map.keys (envValues env) `shouldNotContain` ["global:f"]

        it "moves back up to global" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "fn = (f: f); fn 1; a = 1"
          Map.keys (envValues env) `shouldContain` ["global:a"]

        it "does not leak state" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "fn = (f: f); fn 1; a = 1"
          Map.keys (envValues env) `shouldNotContain` ["global:f"]

        it "let-in does not leak state" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "let x = 2: x"
          Map.keys (envValues env) `shouldNotContain` ["global:x"]

        it "fold does not leak state" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "fold (acc x: acc) 1 [1]"
          Map.keys (envValues env) `shouldNotContain` ["global:x"]
          Map.keys (envValues env) `shouldNotContain` ["global:acc"]

        it "does not leak nested scope" $ \stdLibEnv ->
          evaluate (evals (parseExprs "fn (x: (let b = 1 in b) b)")) `shouldThrow` anyException

        it "multiple assignments" $ \stdLibEnv ->
          evals (parseExprs "a [] = 1; a b = 2; a []") `shouldBe` IntVal 1

        it "cons" $ \stdLibEnv ->
          evals (parseExprs "1 :: []") `shouldBe` ListVal [IntVal 1]

      describe "Tuple" $ do
        it "destructuring tuple returns itself" $ \stdLibEnv ->
          eval (parseExpr "( a, b ) = ( 1, 2 )") `shouldBe` (TupleVal [IntVal 1, IntVal 2])

        it "destructuring tuple pushes to scope" $ \stdLibEnv ->
          evals (parseExprs "( a, b ) = ( 1, 2 ); a + b") `shouldBe` IntVal 3

        it "destructuring nested tuple" $ \stdLibEnv ->
          eval (parseExpr "( a, ( b, c ) ) = ( 1, ( 2, 3 ) )") `shouldBe` (TupleVal [IntVal 1, (TupleVal [IntVal 2, IntVal 3])])

        it "destructuring nested tuple pushes to scope" $ \stdLibEnv ->
          evals (parseExprs "( a, ( b, c ) ) = ( 1, ( 2, 3 ) ); a + b + c") `shouldBe` IntVal 6

        it "underscore ignores" $ \stdLibEnv ->
          evals (parseExprs "( _, b ) = ( 1, 2 ); b") `shouldBe` IntVal 2

        it "underscore ignores" $ \stdLibEnv ->
          evaluate (evals (parseExprs "( _, b ) = ( 1, 2 ); _")) `shouldThrow` anyException

        it "destructuring tuple too many on left side fails" $ \stdLibEnv ->
          evaluate (eval (parseExpr "( a, b, c ) = ( 1, 2 )")) `shouldThrow` anyException

        it "destructuring tuple too many on right side fails" $ \stdLibEnv ->
          evaluate (eval (parseExpr "( a, b ) = ( 1, 2, 3 )")) `shouldThrow` anyException

        it "destructuring tuple with atom on right side" $ \stdLibEnv ->
          evals (parseExprs "c = 1; ( a, b ) = ( 1, c )") `shouldBe` (TupleVal [IntVal 1, IntVal 1])

        it "destructuring tuple with expression on right side" $ \stdLibEnv ->
          evals (parseExprs "( a, b ) = ( 1, (c = 1) )") `shouldBe` (TupleVal [IntVal 1, IntVal 1])

        it "destructuring in lambda (two args)" $ \stdLibEnv ->
          evals (parseExprs "(( a,b ): a + b) ( 1,2 )") `shouldBe` IntVal 3

        it "destructuring in lambda (four args)" $ \stdLibEnv ->
          evals (parseExprs "(( a,b,c,d ): a + b + c  + d) ( 1,2,3,4 )") `shouldBe` IntVal 10

        it "destructuring in lambda (nested)" $ \stdLibEnv ->
          evals (parseExprs "(( a,( b,c ) ): a + b + c) ( 1,( 2,3 ) )") `shouldBe` IntVal 6

      describe "Range" $ do
        it "range" $ \stdLibEnv ->
          eval (parseExpr "(1..3)") `shouldBe` (ListVal [IntVal 1, IntVal 2, IntVal 3])

        it "range on atom" $ \stdLibEnv ->
          evals (parseExprs "a = 3; (1..a)") `shouldBe` (ListVal [IntVal 1, IntVal 2, IntVal 3])

      describe "Internal functions" $ do
        it "head" $ \stdLibEnv ->
          eval (parseExpr "(InternalFunction head [2, 3])") `shouldBe` DataVal "Maybe" "Some" [IntVal 2]

        it "head returns Nothing on empty list" $ \stdLibEnv ->
          eval (parseExpr "(InternalFunction head [])") `shouldBe` DataVal "Maybe" "None" []

        it "sort" $ \stdLibEnv ->
          eval (parseExpr "(InternalFunction sort [3, 2])") `shouldBe` ListVal [IntVal 2, IntVal 3]

        it "zipWith" $ \stdLibEnv ->
          eval (parseExpr "(InternalFunction zipWith [(x y: [x, y]), [1,2, 3], [3, 2]])") `shouldBe` ListVal [ListVal [IntVal 1, IntVal 3], ListVal [IntVal 2, IntVal 2]]

      describe "Runtime type system" $ do
        xit "Can't declare Integer as String" $ \stdLibEnv ->
          evaluate (eval (parseExpr "i = 1 # String")) `shouldThrow` anyException

        xit "Can't declare integer as string typedef" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a # String; a = 1")) `shouldThrow` anyException

        xit "Too many arguments in function definition" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a # Integer: Integer; a b c = b + c ")) `shouldThrow` anyException

        it "Binding definition pushed to env" $ \stdLibEnv ->
          evalIn emptyEnv (parseExpr "a # Integer")
            `shouldSatisfy` ( \case
                                (_, env) ->
                                  Map.lookup "a" (typeSigs env)
                                    == Just (TypeSig {typeSigName = Just "a", typeSigTraitBinding = Nothing, typeSigIn = [], typeSigReturn = IntType})
                            )

        it "Called with wrong type" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a # Integer: Integer; a b = b + 1; a \"s\"")) `shouldThrow` anyException

        it "Called with wrong type second argument" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a # Integer, Integer: Integer; a b c = b + 1; a 1 \"s\"")) `shouldThrow` anyException

        it "Correct types, two different" $ \stdLibEnv ->
          evals (parseExprs "a # Integer, String: Integer; a b c = b + 1; a 1 \"s\"") `shouldBe` IntVal 2

        it "Called with wrong type second argument, different types" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a # String, Integer: Integer; a b c = c + 1; a 1 \"s\"")) `shouldThrow` anyException

      describe "Pattern matching" $ do
        it "can fall through" $ \stdLibEnv ->
          evals (parseExprs "a [] = 1; a b = [2]; a 3") `shouldBe` ListVal [IntVal 2]

        it "can fall through on second argument" $ \stdLibEnv ->
          evals (parseExprs "a b [] = 1; a b c = c; a 1 3") `shouldBe` IntVal 3

        it "empty list should only match empty list" $ \stdLibEnv ->
          evals (parseExprs "a [] = [0]; a b = [2]; a [1]") `shouldBe` ListVal [IntVal 2]

        it "matches specific integer value" $ \stdLibEnv ->
          evals (parseExprs "f 1 = 2; f s = 3; f 1") `shouldBe` IntVal 2

        it "falls through non-matching integer value" $ \stdLibEnv ->
          evals (parseExprs "f 1 = 2; f s = 3; f 2") `shouldBe` IntVal 3

        it "value constructor" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "a None = 1; a (Some 1) = 2; a None"
          val `shouldBe` IntVal 1

        it "value constructor call fall through" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "a None = 1; a (Some b) = b; a (Some 2)"
          val `shouldBe` IntVal 2

        it "value constructor non-first" $ \stdLibEnv -> do
          let (val, env) = evalsIn stdLibEnv $ parseExprs "maybe2 default f None = default; maybe2 1 1 None"
          val `shouldBe` IntVal 1

        it "value constructor non-first, multiple definitions" $ \stdLibEnv -> do
          let expr =
                [iii|
                  maybe2 default f None = default;
                  maybe2 default f (Some x) = x;
                  maybe2 1 1 (Some 2)
                |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs expr
          val `shouldBe` IntVal 2

        it "value constructor non-first, multiple definitions" $ \stdLibEnv -> do
          let expr =
                [iii|
                  maybe2 default f None = default;
                  maybe2 default f (Some x) = x;
                  maybe2 1 1 None
                |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs expr
          val `shouldBe` IntVal 1

      describe "Modules" $ do
        it "evals module" $ \stdLibEnv ->
          evals (parseExprs "module A { 1 }") `shouldBe` IntVal 1

        xit "can't call without namespacing outside of module" $ \stdLibEnv ->
          evaluate (evals (parseExprs "module A { s = 1 }; s")) `shouldThrow` anyException

        xit "can call with namespacing outside of module" $ \stdLibEnv ->
          evals (parseExprs "module A { s = 1 }; A.s") `shouldBe` IntVal 1

        it "can call without namespacing inside module" $ \stdLibEnv ->
          evals (parseExprs "module A { s = 1; s }") `shouldBe` IntVal 1

      describe "String interpolation" $
        it "atoms and integer" $ \stdLibEnv -> do
          evals (parseExprs "a=1; \"before#{a+1}after\"") `shouldBe` (StringVal "before2after")

      describe "Cons list" $ do
        it "destructuring list in let-in" $ \stdLibEnv ->
          evals (parseExprs "let (x::xs) = [1,2,3]: x") `shouldBe` IntVal 1

        it "function destructuring x" $ \stdLibEnv ->
          evals (parseExprs "a (x::xs) = x; a [1,2,3]") `shouldBe` IntVal 1

        it "function destructuring xs" $ \stdLibEnv ->
          evals (parseExprs "a (x::xs) = xs; a [1,2,3]") `shouldBe` (ListVal [IntVal 2, IntVal 3])

        it "function destructuring empty xs" $ \stdLibEnv ->
          evals (parseExprs "a (x::xs) = xs; a [1]") `shouldBe` (ListVal [])

        it "function destructuring doesn't match on empty list" $ \stdLibEnv ->
          evaluate (evals (parseExprs "a (x::xs) = x; a []")) `shouldThrow` anyException

      describe "Recursion" $
        it "recurs" $ \stdLibEnv -> do
          evals (parseExprs "a b = (b == 0) ? [] : (a (b-1)); a 1") `shouldBe` (ListVal [])

      describe "Data" $ do
        it "Defines and constructs zero-argument value" $ \stdLibEnv ->
          evals (parseExprs "data Bool = False | True; True") `shouldBe` (DataVal "Bool" "True" [])

        it "Defines and constructs multi-argument value" $ \stdLibEnv ->
          evals (parseExprs "data Shape = Circle Float Float Float | Rectangle Float Float Float Float; (Rectangle 1.0 1.0 1.0)") `shouldBe` (DataVal "Shape" "Rectangle" [FloatVal 1.0, FloatVal 1.0, FloatVal 1.0])

        it "Needs to match argument types - success" $ \stdLibEnv ->
          evals (parseExprs "data Point = Point Float Float; Point 1.0 1.0") `shouldBe` (DataVal "Point" "Point" [FloatVal 1.0, FloatVal 1.0])

        it "Needs to match argument types - failure" $ \stdLibEnv ->
          evaluate (evals (parseExprs "data Point = Point Float Float; Point 1 1")) `shouldThrow` anyException

        it "Can't provide too many arguments" $ \stdLibEnv ->
          evaluate (evals (parseExprs "data Point = Point Float Float; Point 1.0 1.0 1.0")) `shouldThrow` anyException

        it "destructures" $ \stdLibEnv -> do
          let (val, _) = evalsIn stdLibEnv (parseExprs "a (Some b) = b; a (Some 1)")
          val `shouldBe` IntVal 1

      describe "Case expression" $ do
        it "two boolean cases" $ \stdLibEnv ->
          evals (parseExprs "case true: | true: 1 | false: 0") `shouldBe` IntVal 1

        it "handles more difficult expressions" $ \stdLibEnv -> do
          let (val, _) = evalsIn stdLibEnv (parseExprs "case (Some 1): | (Some x): x | None: 0")
          val `shouldBe` IntVal 1

      describe "Trait" $ do
        it "can create trait" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable: | xmap # (a: b): a: b")
          Map.keys (envValues env) `shouldContain` ["global:Mappable"]

        it "handles type variables" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Functor f: | fmap # (a: b), f a: f b")
          Map.keys (envValues env) `shouldContain` ["global:Functor"]

      describe "Implementation" $ do
        it "can create implementation" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable: | xmap # (a: b), a: b; implement Mappable for Maybe: | xmap f a = None")
          Map.keys (envValues env) `shouldContain` ["global:xmap"]

        it "can create implementation with multiple definitions" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable: | xmap # (a: b), a: b; implement Mappable for Maybe: | xmap _ None = None | xmap f (Some x) = f x")
          length (sequenceA $ Map.lookup "global:xmap" (envValues env)) `shouldBe` 2

        it "Needs to implement the right function" $ \stdLibEnv -> do
          let expr =
                [iii|
                    trait Mappable: | xmap \# (a: b), a: b;
                    implement Mappable for Maybe:
                    | ymap _ None = None
                |]
          let (val, env) = (evalsIn emptyEnv (parseExprs expr))
          evaluate env `shouldThrow` anyException

        it "Uses the right function" $ \stdLibEnv -> do
          let expr =
                [iii|
                  trait Applicative2 f:
                  | ap2 \# f (a: b), f a: f b;
                  implement Applicative2 for Maybe:
                  | ap2 _ _ = None;
                  implement Applicative2 for Result:
                  | ap2 _ (Err a) = Err a;
                  ap (Ok (x: x*2)) (Err 1)
                |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs expr
          val `shouldBe` DataVal "Result" "Err" [IntVal 1]

        it "passes trait function type definition" $ \stdLibEnv -> do
          let expr =
                [iii|
                  trait Applicative2 f:
                  | ap2 \# f (a: b), f a: f b;
                  implement Applicative2 for Maybe:
                  | ap2 _ _ = None
                |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs expr
          case Map.lookup "global:ap2" (envValues env) of
            Just [FunctionVal ts _ _ _] -> ts `shouldBe` (TypeSig {typeSigName = Just "ap2", typeSigTraitBinding = Just "Maybe", typeSigIn = [], typeSigReturn = TypeConstructorType "Applicative2" (FunctionType [AnyType] AnyType)})
            _ -> expectationFailure "No"
