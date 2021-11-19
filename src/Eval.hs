{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval where

import Control.Exception
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
import Syntax
import TypeCheck

instance Evaluatable Val where
  evalIn env val = (val, env)
  toExpr val = undefined

instance Evaluatable Expr where
  toExpr expr = expr
  evalIn env (NamedTypeSig ts) = (Undefined, typeSigToEnv env ts)
  evalIn env (PIf (PBool True) t _) = evalIn env t
  evalIn env (PIf (PBool False) _ f) = evalIn env f
  evalIn env (PIf condition ifTrue ifFalse) =
    let (val, env') = evalIn env condition
     in if val == Boolean True then evalIn env' ifTrue else evalIn env' ifFalse
  evalIn env (Lambda ts args e) = (Function ts env args e, env)
  evalIn env (InternalFunction f args) = internalFunction env f args
  evalIn env (App e1 e2) = apply (withScope env) e1 e2
  evalIn env (PJust ts s) = (LJust (fst $ evalIn env s), env)
  evalIn env (PNothing) = (LNothing, env)
  evalIn env (Atom ts atomId) = case inScope env atomId of
    Just [definition] -> (definition, env)
    Just definitions -> case last definitions of
      Function {} -> (Pattern definitions, env)
      lastDef -> (lastDef, env)
    Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
  evalIn env (PString n) = (StringVal n, env)
  evalIn env (PFloat n) = (FloatVal n, env)
  evalIn env (PInteger n) = (IntVal n, env)
  evalIn env (PDictUpdate baseDict updateDict) =
    let (DictVald1) = fst $ evalIn env baseDict
        (DictVald2) = fst $ evalIn env updateDict
     in (DictVal $ Map.union d2 d1, env)
  evalIn env (DictAccess k dict) =
    let (DictValm) = fst $ evalIn env dict
        kv = fst $ evalIn env k
     in (fromJust (Map.lookup kv m), env)
  evalIn env (PDictKey k) = (DictKey k, env)
  evalIn env (PDict ts pairs) =
    let fn (k, v) = (fst $ evalIn env k, fst $ evalIn env v)
     in (DictVal $ Map.fromList $ map fn pairs, env)
  evalIn env (PRange ts lBoundExp uBoundExp) =
    let lBound = fst $ evalIn env lBoundExp
        uBound = fst $ evalIn env uBoundExp
     in case (lBound, uBound) of
          (IntVal l, IntVal u) -> (List $ map IntVal [l .. u], env)
          _ -> error "Invalid range"
  evalIn env (PList ts es) = (List $ map (fst . evalIn env) es, env)
  evalIn env (PTuple ts es) = (Tuple $ map (fst . evalIn env) es, env)
  evalIn env (PBool n) = (Boolean n, env)
  evalIn env (Binop Assign (PTuple ts1 bindings) (PTuple ts2 vs)) =
    let evaledValues = map (fst . evalIn env) vs
        newEnv = extendWithTuple env bindings evaledValues
     in if length bindings /= length evaledValues
          then throw $ EvalException "Destructuring failed. Mismatched parameter count"
          else (Tuple evaledValues, newEnv)
  evalIn env (Binop Concat e1 e2) =
    let (List xs, _) =
          -- trace ("E1: " ++ show (evalIn env e1)) $
          evalIn env e1
        (List ys, _) =
          -- trace ("E2: " ++ show (evalIn env e2)) $
          evalIn env e2
        e = error "Invalid"
     in (List $ xs ++ ys, env)
  evalIn env (Binop Pipe e1 e2) = apply (withScope env) e2 e1
  evalIn env (Binop Assign (Atom ts a) v) =
    let env'' = extend env' a AnyType v
        (value, env') = evalIn env v
     in (value, env'')
  evalIn env (Binop op e1 e2) =
    let (v1, _) = evalIn env e1
        (v2, _) = evalIn env e2
        x = evalOp op
     in (v1 `x` v2, env)
  evalIn env (Cmp op e1 e2) =
    let (v1, _) = evalIn env e1
        (v2, _) = evalIn env e2
        x = cmpOp op
     in (v1 `x` v2, env)
  evalIn env PNoop = (Undefined, env)
  evalIn _ a = error $ "failed to find match in evalIn" ++ show a

extendWithTuple :: Env -> [Expr] -> [Val] -> Env
extendWithTuple env bindings vs =
  -- trace ("bindings:" ++ List.intercalate ", " (map show bindings) ++ "\n values: " ++ List.intercalate ", " (map show vs)) $
  let foldFun :: Env -> (Expr, Val) -> Env
      foldFun accEnv (atom, val) = case atom of
        Atom ts atomId -> extend accEnv atomId AnyType val
        PTuple ts nBindings -> case val of
          (Tuple vls) -> extendWithTuple accEnv nBindings vls
          _ -> error "Trying to destructure into non-atom"
        _ -> error "Trying to destructure into non-atom"
   in foldl foldFun env (zip bindings vs)

extend :: Evaluatable e => Env -> Id -> LangType -> e -> Env
extend env "_" _ _ = env
extend env id expectedType ex =
  if expectedType == AnyType || gotType == expectedType
    then env {envValues = Map.insertWith (flip (++)) key [val] (envValues env)}
    else throw . RuntimeException $ "Expected type " ++ show expectedType ++ ", got " ++ show gotType
  where
    gotType = toLangType val
    val = fst $ evalIn env ex
    key = last (envScopes env) ++ ":" ++ id

internalFunction :: Evaluatable e => Env -> Id -> e -> (Val, Env)
internalFunction env f argsList = case evaledArgsList of
  List evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    evaledArgsList = fst $ evalIn env argsList
    fun "fold" (fun : init : List xs : _) =
      let foldFun acc x = trace ("calling f with x = " ++ show x) $ fst $ evalIn env $ App (App (funToExpr fun) acc) x
       in foldl foldFun init xs
    fun "zipWith" (fun : List xs : List ys : _) =
      let zipFun :: Evaluatable e => e -> e -> Val
          zipFun x y = fst $ evalIn env $ App (App (funToExpr fun) x) y
       in List $ zipWith zipFun xs ys
    fun "head" xs = case xs of
      [] -> LNothing
      (x : _) -> LJust x
    fun "dictToList" (dict : _) = case dict of
      (DictVald) -> List $ map (\(k, v) -> (Tuple [k, v])) (Map.toList d)
    fun "sort" xs = List . List.sort $ xs
    fun x r = error ("No such function " ++ show x ++ show r)
    funToExpr (Function ts env args e) = (Lambda ts args e)

apply :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
apply env e1 e2 = case evalIn env e1 of
  (Pattern definitions, env') -> case List.find (patternMatch passedArg) definitions of
    Just (Function ts _ args e3) -> runFun env' ts args e2 e3
    Nothing -> error "Pattern match fail"
  (Function ts _ args e3, env') -> runFun env' ts args e2 e3
  (val, env) -> trace ("Cannot apply val: " ++ show val ++ "!") $ error ("Cannot apply value" ++ show env)
  where
    (passedArg, _) = evalIn env e2

patternMatch :: Val -> Val -> Bool
patternMatch passedArg definition = case definition of
  Function _ _ (_, expectedArgExp : _) _ ->
    let passedArgType = toLangType passedArg
        expectedType = toLangType expectedArgExp
        typesMatch =
          expectedType == AnyType
            || expectedType == AtomType
            || passedArgType == expectedType

        patternMatches (PList _ []) (List []) = True
        patternMatches (PList _ _) _ = False
        patternMatches (PInteger e) (IntVal v) = e == v
        patternMatches (PFloat e) (FloatVal v) = e == v
        patternMatches _ _ = True
     in --trace ("calling f with x = " ++ show passedArg ++ " - " ++ show expectedArgExp) $
        typesMatch && patternMatches expectedArgExp passedArg

runFun :: (Evaluatable e1, Evaluatable e2) => Env -> TypeSig -> ArgsList -> e1 -> e2 -> (Val, Env)
runFun env ts argsList expr funExpr =
  let (allArgs, args@(arg : remainingArgs')) = argsList
      -- NOTE: move extension of 'whatever' to separate function
      newEnv = case arg of
        (Atom _ts atomId) -> extend env atomId (expectedType env ts $ length args) expr
        (PTuple _ts bindings) -> case fst $ evalIn env expr of
          (Tuple vals) -> extendWithTuple env bindings vals
          _ -> error "Non-tuple value received for tuple destructuring"
   in if null remainingArgs'
        then evalIn newEnv funExpr
        else evalIn newEnv (Lambda ts (allArgs, remainingArgs') funExpr)

eval :: Evaluatable e => e -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Evaluatable e => Env -> e -> (Val, Env)
evalInEnv = evalIn

evals :: Evaluatable e => [e] -> Val
evals exprs = fst $ foldl fl (Undefined, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
