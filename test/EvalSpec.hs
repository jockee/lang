{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module EvalSpec where

import Control.Exception
import Data.Map qualified as Map
import Data.String.Interpolate (i, __i)
import Debug.Trace
import Eval
import Lang
import Parser
import Test.Hspec
import TypeCheck
import Types

spec :: Spec
spec = beforeAll (let !std = evaledStdLibEnv in std) $
  describe "Eval" $ do
    describe "Functions/scoping" $ do
      it "a named function can take one argument and return it" $ \stdLibEnv ->
        evals (parseExprs "fn x = x; fn 1") `shouldBe` IntVal 1

      it "lambda partially applied" $ \stdLibEnv ->
        evals (parseExprs "c = 2; (x b: x + b) 2")
          `shouldSatisfy` ( \case
                              FunctionVal lambdaEnv _ _ _ -> Map.keys (lambdaEnvBindings lambdaEnv) == ["c", "x"]
                              _ -> False
                          )

      it "lambda partially applied" $ \stdLibEnv ->
        evals (parseExprs "a b = c (x b: x + b); c f y = 1; a 2")
          `shouldSatisfy` ( \case
                              FunctionVal lambdaEnv _ _ _ -> Map.keys (lambdaEnvBindings lambdaEnv) == ["f"]
                              _ -> False
                          )
      it "funtion that receives a lambda doesn't have access the lambdas scope" $ \stdLibEnv ->
        evals (parseExprs "a = (x b: x + b); c y x = y 1; c a")
          `shouldSatisfy` ( \case
                              FunctionVal lambdaEnv _ _ _ -> Map.keys (lambdaEnvBindings lambdaEnv) == ["y"]
                              _ -> False
                          )

      it "a lambda contains its env" $ \stdLibEnv ->
        evals (parseExprs "a b = let c = 1: d (x: x+c); d f = f 1; a 999") `shouldBe` IntVal 2

      it "only functions from lambda-spawn site are available on application within lambda" $ \stdLibEnv ->
        evals (parseExprs "module A { c _ = 2; a f = f 3 }; c _ = 1; A.a (x: x + (c 2))") `shouldBe` IntVal 4

    describe "Modules" $ do
      it "pushes to env" $ \stdLibEnv -> do
        let (_, env) = evalsIn emptyEnv (parseExprs "module A { a = 1 }")
        case inScope env "A.a" of
          [] -> expectationFailure "No"
          _ -> return ()

      it "evals module" $ \stdLibEnv ->
        evals (parseExprs "module A { 1 }") `shouldBe` IntVal 1

      it "can't call without namespacing outside of module" $ \stdLibEnv ->
        evaluate (evals (parseExprs "module A { s = 1 }; s")) `shouldThrow` anyException

      it "can call with namespacing outside of module" $ \stdLibEnv ->
        evals (parseExprs "module A { s = 1 }; A.s") `shouldBe` IntVal 1

      it "can call with nested namespacing outside of module" $ \stdLibEnv ->
        evals (parseExprs "module A { module B { s = 1 } }; A.B.s") `shouldBe` IntVal 1

      it "can call without namespacing inside module" $ \stdLibEnv ->
        evals (parseExprs "module A { s = 1; p x = x + 1; p 1 }") `shouldBe` IntVal 2

      it "can call another function without namespacing inside module" $ \stdLibEnv ->
        evals (parseExprs "module A { s f = 1; p x = (s 2) + x; p 1 }") `shouldBe` IntVal 2

      it "can call without namespacing inside module" $ \stdLibEnv ->
        evals (parseExprs "module A { a x = 1; b i = a i }; A.b 1") `shouldBe` IntVal 1

      it "can call without namespacing inside module [STDLIB]" $ \stdLibEnv -> do
        let (val, _) = evalsIn stdLibEnv (parseExprs "module Dict2 { toList2 xs = (HFI dictToList [xs]); keys dict = map ((a, b): a) (toList2 dict) }; Dict2.keys {a:1}")
        val `shouldBe` ListVal [StringVal "a"]

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

        it "destructuring list returns itself" $ \stdLibEnv ->
          eval (parseExpr "[ a, b ] = [ 1, 2 ]") `shouldBe` ListVal [IntVal 1, IntVal 2]

        it "destructuring list pushes to scope" $ \stdLibEnv ->
          evals (parseExprs "[ a, b ] = [ 1, 2 ]; a + b") `shouldBe` IntVal 3

        it "destructuring nested list" $ \stdLibEnv ->
          eval (parseExpr "[ a, [ b, c ] ] = [ 1, [ 2, 3 ] ]") `shouldBe` ListVal [IntVal 1, ListVal [IntVal 2, IntVal 3]]

        it "destructuring nested list pushes to scope" $ \stdLibEnv ->
          evals (parseExprs "[ a, [ b, c ] ] = [ 1, [ 2, 3 ] ]; a + b + c") `shouldBe` IntVal 6

        it "underscore ignores" $ \stdLibEnv ->
          evals (parseExprs "[ _, b ] = [ 1, 2 ]; b") `shouldBe` IntVal 2

        it "underscore ignores" $ \stdLibEnv ->
          evaluate (evals (parseExprs "[ _, b ] = [ 1, 2 ]; _")) `shouldThrow` anyException

        it "destructuring list too many on left side fails" $ \stdLibEnv ->
          evaluate (eval (parseExpr "[ a, b, c ] = [ 1, 2 ]")) `shouldThrow` anyException

        it "destructuring list too many on right side fails" $ \stdLibEnv ->
          evaluate (eval (parseExpr "[ a, b ] = [ 1, 2, 3 ]")) `shouldThrow` anyException

        it "destructuring list with atom on right side" $ \stdLibEnv ->
          evals (parseExprs "c = 1; [ a, b ] = [ 1, c ]") `shouldBe` ListVal [IntVal 1, IntVal 1]

        it "destructuring list with expression on right side" $ \stdLibEnv ->
          evals (parseExprs "[ a, b ] = [ 1, ( c = 1 ) ]") `shouldBe` ListVal [IntVal 1, IntVal 1]

        it "destructuring in lambda (two args)" $ \stdLibEnv ->
          evals (parseExprs "([ a,b ]: a + b) [ 1,2 ]") `shouldBe` IntVal 3

        it "destructuring in lambda (four args)" $ \stdLibEnv ->
          evals (parseExprs "([ a,b,c,d ]: a + b + c  + d) [ 1,2,3,4 ]") `shouldBe` IntVal 10

        it "destructuring in lambda (nested)" $ \stdLibEnv ->
          evals (parseExprs "([ a,[ b,c ] ]: a + b + c) [ 1,[ 2,3 ] ]") `shouldBe` IntVal 6

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

      it "fmap pipe [STDLIB]" $ \stdLibEnv -> do
        let (val, _) = evalsIn stdLibEnv (parseExprs "(n: n * 2) ||> (Some 1)")
        val `shouldBe` DataVal "Maybe" "Some" [IntVal 2]

      it "dict key as function in fmap [STDLIB]" $ \stdLibEnv -> do
        let (val, _) = evalsIn stdLibEnv (parseExprs "fmap .body (Some {body: 1})")
        val `shouldBe` DataVal "Maybe" "Some" [IntVal 1]

    describe "STDLIB" $ do
      it "fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "fold (acc x: acc * x) 1 [2, 3]")
        val `shouldBe` IntVal 6

      it "zip" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "zip [1,2] [3,4]")
        val `shouldBe` ListVal [TupleVal [IntVal 1, IntVal 3], TupleVal [IntVal 2, IntVal 4]]

      it "inline partially applied mapping fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "(f: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2)")
        val `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "inline fully applied mapping fold function" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "(f xs: fold (acc x: acc ++ [f x]) [] [1,2]) (x: x*2) [1,2]")
        val `shouldBe` ListVal [IntVal 2, IntVal 4]

      it "reverse" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "reverse [1, 2]")
        val `shouldBe` ListVal [IntVal 2, IntVal 1]

      it "empty?" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "empty? [1, 2]")
        val `shouldBe` BoolVal False

      it "empty?" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "empty? []")
        val `shouldBe` BoolVal True

      it "takeWhile" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "takeWhile (x: x < 3) [1,2,3,4,1,2,3,4]")
        val `shouldBe` ListVal [IntVal 1, IntVal 2]

      it "dropWhile" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "dropWhile (x: x < 3) [1,2,3,4,5,1,2,3]")
        val `shouldBe` ListVal [IntVal 3, IntVal 4, IntVal 5, IntVal 1, IntVal 2, IntVal 3]

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

      it "length" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "length [1,2]")
        val `shouldBe` IntVal 2

      it "sort" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "sort [3,1,2]")
        val `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

      it "take" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "take 3 [1,2,3,4,5]")
        val `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

      it "drop" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "drop 2 [1,2,3,4,5]")
        val `shouldBe` ListVal [IntVal 3, IntVal 4, IntVal 5]

      it "toList" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "Dict.toList {a: 1, b: 2}")
        val `shouldBe` ListVal [TupleVal [StringVal "a", IntVal 1], TupleVal [StringVal "b", IntVal 2]]

      it "values" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "Dict.values {a: 1, b: 2}")
        val `shouldBe` ListVal [IntVal 1, IntVal 2]

      it "keys" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "Dict.keys {a: 1, b: 2}")
        val `shouldBe` ListVal [StringVal "a", StringVal "b"]

      it "merge" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "Dict.merge {a: 1} {b: 2}")
        val `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1), (DictKey "b", IntVal 2)])

      it "toDict" $ \stdLibEnv -> do
        let (val, _) = evalIn stdLibEnv (parseExpr "toDict [(\"a\", 1)]")
        val `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1)])

      describe "Stdlib Types" $ do
        it "fmap" $ \stdLibEnv -> do
          let (val, _) = evalIn stdLibEnv (parseExpr "fmap (x: x*2) (Some 1)")
          val `shouldBe` DataVal "Maybe" "Some" [2]

        it "ap" $ \stdLibEnv -> do
          let (val, _) = evalIn stdLibEnv (parseExpr "ap (Some (x: x*2)) (Some 1)")
          val `shouldBe` DataVal "Maybe" "Some" [2]

        it "bind" $ \stdLibEnv -> do
          let (val, _) = evalIn stdLibEnv (parseExpr "bind (Some 1) (x: (Some (x * 2)))")
          val `shouldBe` DataVal "Maybe" "Some" [2]

    describe "Function composition" $ do
      xit "using pipe" $ \stdLibEnv ->
        evals (parseExprs "length s f= f; (.body |> length) {body: \"asd\"}") `shouldBe` DataVal "Maybe" "Some" [IntVal 2]

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
        eval (parseExpr "{a: 1}") `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1)])

      it "dict lookup using dotkey on atom" $ \stdLibEnv ->
        evals [parseExpr "dict = {a: 1, b: 2}", parseExpr ".a dict"] `shouldBe` IntVal 1

      it "dict lookup using dotkey on dict" $ \stdLibEnv ->
        eval (parseExpr ".a {a: 1, b: 2}") `shouldBe` IntVal 1

      it "dict lookup using dict.key on atom" $ \stdLibEnv ->
        evals [parseExpr "dict = {a: 1, b: 2}", parseExpr "dict.a"] `shouldBe` IntVal 1

      it "dict update" $ \stdLibEnv ->
        eval (parseExpr "{ {a: 0} | a: 1 }") `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1)])

      it "dict update new key" $ \stdLibEnv ->
        eval (parseExpr "{ {a: 0} | b: 1 }") `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 0), (DictKey "b", IntVal 1)])

      it "dict update on atom" $ \stdLibEnv ->
        evals [parseExpr "dict = {b: 2}", parseExpr "{ dict | b: 1 }"] `shouldBe` DictVal (Map.fromList [(DictKey "b", IntVal 1)])

    it "dict dynamic key" $ \stdLibEnv ->
      evals (parseExprs "a = \"s\"; { a => 1 }") `shouldBe` DictVal (Map.fromList [(DictKey "s", IntVal 1)])

    it "dict update dynamic key" $ \stdLibEnv ->
      evals (parseExprs "a = \"s\"; { {a: 1} | a => 2 }") `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1), (DictKey "s", IntVal 2)])

    it "destructuring dict basic" $ \stdLibEnv ->
      evals (parseExprs "let {a: b} = {a: 2}: b") `shouldBe` IntVal 2

    it "destructuring dict, requiring matching other values - succeeding" $ \stdLibEnv ->
      evals (parseExprs "let {a: b, c: 1} = {a: 2, c: 1}: b") `shouldBe` IntVal 2

    it "destructuring dict, requiring matching other values - failing" $ \stdLibEnv ->
      evaluate (evals (parseExprs "let {a: b, c: 1} = {a: 2, c: 2}: b")) `shouldThrow` anyException

    describe "General" $ do
      it "adds to global scope [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "folder = 1"
        case inScope env "folder" of
          [] -> expectationFailure "No"
          _ -> return ()

      it "assignment in lambda does not leak [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "fn = (x: f = 1); fn 1"
        case inScope env "f" of
          [] -> return ()
          _ -> expectationFailure "No"

      -- it "moves back up to global" $ \stdLibEnv -> do
      --   let (val, env) = evalsIn emptyEnv $ parseExprs "fn = (f: f); fn 1; a = 1"
      --   length (envBindings env) `shouldBe` 1

      it "does not leak state [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "fn = (f: f); fn 1; a = 1"
        case inScope env "f" of
          [] -> return ()
          _ -> expectationFailure "No"

      it "let-in does not leak state [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "let x = 2: x"
        case inScope env "x" of
          [] -> return ()
          _ -> expectationFailure "No"

      it "fold does not leak state [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "fold (acc g: acc) 1 [1]"
        case inScope env "g" of
          [] -> return ()
          s -> expectationFailure "x"
        case inScope env "acc" of
          [] -> return ()
          _ -> expectationFailure "acc"

      it "does not leak nested scope" $ \stdLibEnv ->
        evaluate (evals (parseExprs "(x: (let b = 1: b)) 1; b")) `shouldThrow` anyException

      it "does not leak variable local to lambda in the calling function" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a f = offender; b offender = a 1; b 2")) `shouldThrow` anyException
      it "does not leak let binding to other function. doesn't leak downward" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a f = offender; b = let offender = 2: (a 1); b")) `shouldThrow` anyException

      it "does not leak let binding to other function. doesn't leak upward" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a f = b + offender; b = let offender = 2: 1; a 1")) `shouldThrow` anyException

      it "local scope can be passed into lambda" $ \stdLibEnv ->
        evals (parseExprs "b y = (x: x + y) 1; b 1") `shouldBe` IntVal 2

      it "multiple assignments" $ \stdLibEnv ->
        evals (parseExprs "a [] = 1; a b = 2; a []") `shouldBe` IntVal 1

      it "cons" $ \stdLibEnv ->
        evals (parseExprs "1 :: []") `shouldBe` ListVal [IntVal 1]

      it "nested functions can use the same variable name" $ \stdLibEnv -> do
        let fold = "jfold _ initOrAcc [] = initOrAcc; jfold b initOrAcc (k::ks) = let val = (b initOrAcc k): jfold b val ks;"
        let args = "[1,2,3]; s 3"
        let notOverlapping = fold ++ "filter' f xs = jfold (acc uU: (f uU) ? (acc ++ [uU]) : acc) [] xs; s x = filter' (a: a < x)" ++ args -- `uU` is unique
        let overlapping = fold ++ "filter' f xs = jfold (acc xX: (f xX) ? (acc ++ [xX]) : acc) [] xs; s xX = filter' (a: a < xX)" ++ args -- `xX` is not unique
        evals (parseExprs notOverlapping) `shouldBe` ListVal [IntVal 1, IntVal 2]
        evals (parseExprs overlapping) `shouldBe` ListVal [IntVal 1, IntVal 2]

      it "variable accesses where it shouldn't be available" $ \stdLibEnv -> do
        let accessingVarNotInScope = "filter' f xs = f offender; s offender = filter' (a: a < offender) 2; s 1"
        evaluate (evals (parseExprs accessingVarNotInScope)) `shouldThrow` anyException

    describe "Tuple" $ do
      it "destructuring tuple returns itself" $ \stdLibEnv ->
        eval (parseExpr "( a, b ) = ( 1, 2 )") `shouldBe` TupleVal [IntVal 1, IntVal 2]

      it "destructuring tuple pushes to scope" $ \stdLibEnv ->
        evals (parseExprs "( a, b ) = ( 1, 2 ); a + b") `shouldBe` IntVal 3

      it "destructuring nested tuple" $ \stdLibEnv ->
        eval (parseExpr "( a, ( b, c ) ) = ( 1, ( 2, 3 ) )") `shouldBe` TupleVal [IntVal 1, TupleVal [IntVal 2, IntVal 3]]

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
        evals (parseExprs "c = 1; ( a, b ) = ( 1, c )") `shouldBe` TupleVal [IntVal 1, IntVal 1]

      it "destructuring tuple with expression on right side" $ \stdLibEnv ->
        evals (parseExprs "( a, b ) = ( 1, (c = 1) )") `shouldBe` TupleVal [IntVal 1, IntVal 1]

      it "destructuring in lambda (two args)" $ \stdLibEnv ->
        evals (parseExprs "(( a,b ): a + b) ( 1,2 )") `shouldBe` IntVal 3

      it "destructuring in lambda (four args)" $ \stdLibEnv ->
        evals (parseExprs "(( a,b,c,d ): a + b + c  + d) ( 1,2,3,4 )") `shouldBe` IntVal 10

      it "destructuring in lambda (nested)" $ \stdLibEnv ->
        evals (parseExprs "(( a,( b,c ) ): a + b + c) ( 1,( 2,3 ) )") `shouldBe` IntVal 6

    describe "Range" $ do
      it "range" $ \stdLibEnv ->
        eval (parseExpr "(1..3)") `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

      it "range on atom" $ \stdLibEnv ->
        evals (parseExprs "a = 3; (1..a)") `shouldBe` ListVal [IntVal 1, IntVal 2, IntVal 3]

    describe "Runtime type system" $ do
      xit "Can't declare Integer as String" $ \stdLibEnv ->
        evaluate (eval (parseExpr "i = 1 => String")) `shouldThrow` anyException

      xit "Can't declare integer as string typedef" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a => String; a = 1")) `shouldThrow` anyException

      it "Too many arguments in function definition" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a Integer => Integer; a b c = b + c")) `shouldThrow` anyException

      it "Binding definition pushed to env" $ \stdLibEnv ->
        evalIn emptyEnv (parseExpr "a => Integer")
          `shouldSatisfy` ( \case
                              (_, env) ->
                                Map.lookup "a" (typeSigs env)
                                  == Just (TypeSig {typeSigName = Just "a", typeSigModule = Nothing, typeSigTraitBinding = Nothing, typeSigImplementationBinding = Nothing, typeSigIn = [], typeSigReturn = IntType})
                          )

      it "Called with wrong type" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a Integer => Integer; a b = b + 1; a \"s\"")) `shouldThrow` anyException

      xit "Called with wrong type second argument" $ \stdLibEnv ->
        -- XXX: it's not decorating functions with their type defs on call?
        evaluate (evals (parseExprs "a Integer, Integer => Integer; a b c = b + 1; a 1 \"s\"")) `shouldThrow` anyException

      it "Correct types, two different" $ \stdLibEnv ->
        evals (parseExprs "a Integer, String => Integer; a b c = b + 1; a 1 \"s\"") `shouldBe` IntVal 2

      it "Called with wrong type second argument, different types" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a String, Integer => Integer; a b c = c + 1; a 1 \"s\"")) `shouldThrow` anyException

    describe "Pattern matching" $ do
      it "can fall through" $ \stdLibEnv ->
        evals (parseExprs "a [] = 1; a b = [2]; a 3") `shouldBe` ListVal [IntVal 2]

      it "can fall through on second argument" $ \stdLibEnv ->
        evals (parseExprs "a b [] = 1; a b c = c; a 1 3") `shouldBe` IntVal 3

      it "empty list should only match empty list" $ \stdLibEnv ->
        evals (parseExprs "a [] = [0]; a b = [2]; a [1]") `shouldBe` ListVal [IntVal 2]

      it "matches specific string value" $ \stdLibEnv ->
        evals (parseExprs "a \"ko\" = 1; a b = 2; a \"ko\"") `shouldBe` IntVal 1

      it "matches empty string" $ \stdLibEnv ->
        evals (parseExprs "a \"\" = 1; a b = 2; a \"\"") `shouldBe` IntVal 1

      it "matches specific integer value" $ \stdLibEnv ->
        evals (parseExprs "f 1 = 2; f s = 3; f 1") `shouldBe` IntVal 2

      it "falls through non-matching integer value" $ \stdLibEnv ->
        evals (parseExprs "f 1 = 2; f s = 3; f 2") `shouldBe` IntVal 3

      it "value constructor [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "a None = 1; a (Some 1) = 2; a None"
        val `shouldBe` IntVal 1

      it "value constructor call fall through [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "a None = 1; a (Some b) = b; a (Some 2)"
        val `shouldBe` IntVal 2

      it "value constructor non-first [STDLIB]" $ \stdLibEnv -> do
        let (val, env) = evalsIn stdLibEnv $ parseExprs "maybe2 default f None = default; maybe2 1 1 None"
        val `shouldBe` IntVal 1

      it "value constructor non-first, multiple definitions [STDLIB]" $ \stdLibEnv -> do
        let expr =
              [__i|
              maybe2 default f None = default
              maybe2 default f (Some x) = x
              maybe2 1 1 (Some 2)
            |]
        let (val, env) = evalsIn stdLibEnv $ parseExprs expr
        val `shouldBe` IntVal 2

      it "value constructor non-first, multiple definitions [STDLIB]" $ \stdLibEnv -> do
        let expr =
              [__i|
              maybe2 default f None = default
              maybe2 default f (Some x) = x
              maybe2 1 1 None
            |]
        let (val, env) = evalsIn stdLibEnv $ parseExprs expr
        val `shouldBe` IntVal 1
    describe "String interpolation" $
      it "atoms and integer" $ \stdLibEnv ->
        evals (parseExprs "a=1; \"before#{a+1}after\"") `shouldBe` StringVal "before2after"

    describe "Cons list" $ do
      it "destructuring list in let-in" $ \stdLibEnv ->
        evals (parseExprs "let (x::xs) = [1,2,3]: x") `shouldBe` IntVal 1

      it "function destructuring x" $ \stdLibEnv ->
        evals (parseExprs "a (x::xs) = x; a [1,2,3]") `shouldBe` IntVal 1

      it "function destructuring xs" $ \stdLibEnv ->
        evals (parseExprs "a (x::xs) = xs; a [1,2,3]") `shouldBe` ListVal [IntVal 2, IntVal 3]

      it "function destructuring empty xs" $ \stdLibEnv ->
        evals (parseExprs "a (x::xs) = xs; a [1]") `shouldBe` ListVal []

      it "function destructuring doesn't match on empty list" $ \stdLibEnv ->
        evaluate (evals (parseExprs "a (x::xs) = x; a []")) `shouldThrow` anyException

      it "function destructuring falls to cons list" $ \stdLibEnv ->
        evals (parseExprs "a [] = 1; a (x :: xs) = x; a [2]") `shouldBe` IntVal 2

    describe "Recursion" $ do
      it "recurs" $ \stdLibEnv ->
        evals (parseExprs "a b = (b == 0) ? [] : (a (b-1)); a 1") `shouldBe` ListVal []

      it "recursive map" $ \stdLibEnv ->
        evals (parseExprs "xmap _ [] = []; xmap f (x::xs) = (f x) :: (xmap f xs); xmap (x: x * 2) [1,2]") `shouldBe` ListVal [IntVal 2, IntVal 4]

    describe "Data" $ do
      it "Defines and constructs zero-argument value" $ \stdLibEnv ->
        evals (parseExprs "data Bool = False | True; True") `shouldBe` DataVal "Bool" "True" []

      it "Defines and constructs multi-argument value" $ \stdLibEnv ->
        evals (parseExprs "data Shape = Circle Float Float Float | Rectangle Float Float Float Float; (Rectangle 1.0 1.0 1.0)") `shouldBe` DataVal "Shape" "Rectangle" [FloatVal 1.0, FloatVal 1.0, FloatVal 1.0]

      it "Needs to match argument types - success" $ \stdLibEnv ->
        evals (parseExprs "data Point = Point Float Float; Point 1.0 1.0") `shouldBe` DataVal "Point" "Point" [FloatVal 1.0, FloatVal 1.0]

      it "Needs to match argument types - failure" $ \stdLibEnv ->
        evaluate (evals (parseExprs "data Point = Point Float Float; Point 1 1")) `shouldThrow` anyException

      it "Can't provide too many arguments" $ \stdLibEnv ->
        evaluate (evals (parseExprs "data Point = Point Float Float; Point 1.0 1.0 1.0")) `shouldThrow` anyException

      it "destructures [STDLIB]" $ \stdLibEnv -> do
        let (val, _) = evalsIn stdLibEnv (parseExprs "a (Some b) = b; a (Some 1)")
        val `shouldBe` IntVal 1

    describe "Case expression" $ do
      it "two boolean cases" $ \stdLibEnv ->
        evals (parseExprs "case true { true: 1; false: 0 }") `shouldBe` IntVal 1

      it "handles more difficult expressions [STDLIB]" $ \stdLibEnv -> do
        let (val, _) = evalsIn stdLibEnv (parseExprs "case (Some 1) { (Some x): x; None: 0 }")
        val `shouldBe` IntVal 1

    describe "Trait" $ do
      it "can create trait" $ \stdLibEnv -> do
        let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable { xmap (a: b): a => b }")
        case inScope env "Mappable" of
          [] -> expectationFailure "No"
          _ -> return ()

      it "handles type variables" $ \stdLibEnv -> do
        let (_, env) = evalsIn emptyEnv (parseExprs "trait Functor f { fmap (a: b), f a => f b }")
        case inScope env "Functor" of
          [] -> expectationFailure "No"
          _ -> return ()

      describe "Implementation" $ do
        it "can create implementation" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable { xmap (a: b), a => b }; implement Mappable for Maybe { xmap f a = None }")
          case inScope env "xmap" of
            [] -> expectationFailure "No"
            _ -> return ()

        it "can create implementation with multiple definitions" $ \stdLibEnv -> do
          let (_, env) = evalsIn emptyEnv (parseExprs "trait Mappable { xmap (a: b), a => b }; implement Mappable for Maybe { xmap _ None = None; xmap f (Some x) = f x }")
          case inScope env "xmap" of
            [] -> expectationFailure "No"
            _ -> return ()

        it "Uses the right function - last argument [STDLIB]" $ \stdLibEnv -> do
          let baseExpr =
                [i|
              trait Foldable2 f {
                fold2 (a, b: a), a, f b => a
                length2 xs = fold2 (acc x: acc + 1) 0 xs
              }

              implement Foldable2 for List {
                fold2 x init xs = fold x init xs
              }

              implement Foldable2 for String {
                fold2 u init s = fold u init s
              }

              implement Foldable2 for Dict {
                fold2 k init xs = fold k init xs
              }
            |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "length2 [1,2,3]")
          val `shouldBe` IntVal 3
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "length2 \"ok\"")
          val `shouldBe` IntVal 2
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "length2 {a:1, b:2}")
          val `shouldBe` IntVal 2

        it "Uses the right function - non-last argument [STDLIB]" $ \stdLibEnv -> do
          let baseExpr =
                [i|
                  trait Foldable2 f {
                    fold2 (a, b: a), f b, a => a
                    length2 xs = fold2 (acc x: acc + 1) xs 0
                  }

                  implement Foldable2 for List {
                    fold2 x xs init = fold x init xs
                  }

                  implement Foldable2 for String {
                    fold2 k s init = fold k init s
                  }
                |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "length2 [1,2,3]")
          val `shouldBe` IntVal 3
          let (val, !env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "length2 \"ok\"")
          val `shouldBe` IntVal 2

        it "Uses the right function - return value [STDLIB]" $ \stdLibEnv -> do
          let baseExpr =
                [i|
              trait Applicative2 f {
                pure a => f a
              }

              implement Applicative2 for List {
                pure x = [x]
              }

              implement Applicative2 for Maybe {
                pure x = Some x
              }
            |]
          pendingWith "not a completed test"
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "pure 1")
          val `shouldBe` IntVal 2
          let (val, env) = evalsIn stdLibEnv $ parseExprs (baseExpr ++ "maybe ")
          val `shouldBe` IntVal 3

        it "uses trait functions if implements [STDLIB]" $ \stdLibEnv -> do
          let expr =
                [__i|
              trait Applicative2 f {
                ap2 f (a: b), f a => f b;
                ap3 f (a: b), f a => f b;
                ap3 _ _ = 1
              }
              implement Applicative2 for Maybe { ap2 _ _ = None }
              ap3 1 2
            |]
          let (val, env) = evalsIn stdLibEnv $ parseExprs expr
          val `shouldBe` IntVal 1

      it "passes trait function type definition [STDLIB]" $ \stdLibEnv -> do
        let expr =
              [__i|
            trait Applicative2 f { ap2 f (a: b), f a => f b }
            implement Applicative2 for Maybe { ap2 _ _ = None }
          |]
        let (val, env) = evalsIn stdLibEnv $ parseExprs expr
        case inScope env "ap2" of
          [] -> expectationFailure "No"
          [FunctionVal _ ts _ _] ->
            ts
              `shouldBe` (TypeSig {typeSigName = Just "ap2", typeSigModule = Nothing, typeSigTraitBinding = Just "Applicative2", typeSigImplementationBinding = Just "Maybe", typeSigIn = [TraitVariableType "Applicative2" (FunctionType [AnyType] AnyType), TraitVariableType "Applicative2" AnyType], typeSigReturn = TraitVariableType "Applicative2" AnyType})

      describe "JSON" $ do
        it "parseJSON [STDLIB]" $ \stdLibEnv -> do
          let expr = "JSON.parse \"[1, null, 2.3]\""
          let (val, _) = evalsIn stdLibEnv $ parseExprs expr
          val `shouldBe` ListVal [IntVal 1, DataVal "Maybe" "None" [], FloatVal 2.3]

        it "toJSON [STDLIB]" $ \stdLibEnv -> do
          let (val, _) = evalsIn stdLibEnv $ parseExprs "JSON.encode {a: 1, b: 2, c: None}"
          val `shouldBe` StringVal "{\"a\":1,\"b\":2,\"c\":null}"

        it "idempotent [STDLIB]" $ \stdLibEnv -> do
          let (val, _) = evalsIn stdLibEnv $ parseExprs "JSON.parse (JSON.encode {a: 1, b: 2, c: None})"
          val `shouldBe` DictVal (Map.fromList [(DictKey "a", IntVal 1), (DictKey "b", IntVal 2), (DictKey "c", DataVal "Maybe" "None" [])])
