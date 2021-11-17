{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval where

import Control.Exception
import Data.Data
import Data.Foldable (asum)
import Data.Hashable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
import Syntax

instance Hashable Env where
  hashWithSalt k Env {envValues = v} = k `hashWithSalt` Map.keys v

instance Evaluatable Val where
  evalIn env val = (val, env)

instance Evaluatable Expr where
  evalIn env (PIf (PBool True) t _) = evalIn env t
  evalIn env (PIf (PBool False) _ f) = evalIn env f
  evalIn env (PIf condition ifTrue ifFalse) =
    let (val, env') = evalIn env condition
     in if val == Boolean True then evalIn env' ifTrue else evalIn env' ifFalse
  evalIn env (Lambda ids e) = (Function env ids e, env)
  evalIn env (InternalFunction f args) = internalFunction env f args
  evalIn env (App e1 e2) = runFun (withScope env) e1 e2
  evalIn env (Atom atomId) = case inScope env atomId of
    Just a -> (a, env)
    Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
  evalIn env (PString n) = (StringVal n, env)
  evalIn env (PFloat n) = (FloatVal n, env)
  evalIn env (PInteger n) = (IntVal n, env)
  evalIn env (PDictUpdate baseDict updateDict) =
    let (Dictionary d1) = fst $ evalIn env baseDict
        (Dictionary d2) = fst $ evalIn env updateDict
     in (Dictionary $ Map.union d2 d1, env)
  evalIn env (DictAccess k dict) =
    let (Dictionary m) = fst $ evalIn env dict
        kv = fst $ evalIn env k
     in (fromJust (Map.lookup kv m), env)
  evalIn env (PDictKey k) = (DictKey k, env)
  evalIn env (PDict pairs) =
    let fn (k, v) = (fst $ evalIn env k, fst $ evalIn env v)
     in (Dictionary $ Map.fromList $ map fn pairs, env)
  evalIn env (PRange lBoundExp uBoundExp) =
    let lBound = fst $ evalIn env lBoundExp
        uBound = fst $ evalIn env uBoundExp
     in case (lBound, uBound) of
          (IntVal l, IntVal u) -> (List $ map IntVal [l .. u], env)
          _ -> error "Invalid range"
  evalIn env (PList es) = (List $ map (fst . evalIn env) es, env)
  evalIn env (PTuple es) = (Tuple $ map (fst . evalIn env) es, env)
  evalIn env (PBool n) = (Boolean n, env)
  evalIn env (Binop Assign (PTuple bindings) (PTuple vs)) =
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
  evalIn env (Binop Pipe e1 e2) = runFun (withScope env) e2 e1
  evalIn env (Binop Assign (Atom a) v) =
    let env'' = extend env' a v
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

funToExpr :: Val -> Expr
funToExpr (Function env ids e) = (Lambda ids e)

defaultEnvScopes :: [String]
defaultEnvScopes = ["global"]

emptyEnv :: Env
emptyEnv = Env {envValues = Map.empty, envScopes = defaultEnvScopes}

extendWithTuple :: Env -> [Expr] -> [Val] -> Env
extendWithTuple env bindings vs =
  -- trace ("bindings:" ++ List.intercalate ", " (map show bindings) ++ "\n values: " ++ List.intercalate ", " (map show vs)) $
  let foldFun :: Env -> (Expr, Val) -> Env
      foldFun accEnv (atom, val) = case atom of
        Atom atomId -> extend accEnv atomId val
        PTuple nBindings -> case val of
          (Tuple vls) -> extendWithTuple accEnv nBindings vls
          _ -> error "Trying to destructure into non-atom"
        _ -> error "Trying to destructure into non-atom"
   in foldl foldFun env (zip bindings vs)

extend :: Evaluatable e => Env -> Id -> e -> Env
extend env "_" _ = env
extend env id ex =
  Env
    { envValues = Map.insert key (fst $ evalIn env ex) (envValues env),
      envScopes = envScopes env
    }
  where
    key = last (envScopes env) ++ ":" ++ id

resetScope :: Env -> Env
resetScope env = env {envScopes = defaultEnvScopes}

withScope :: Env -> Env
withScope env = newEnv
  where
    newScope = show $ hash newEnv
    newEnv = env {envScopes = List.nub $ envScopes env ++ [newScope]}

inScope :: Env -> String -> Maybe Val
inScope env atomId =
  -- trace ("SCOPEKEYS " ++ show scopeKeys ++ show env) $
  asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = reverse $ map (\k -> k ++ ":" ++ atomId) $ envScopes env

internalFunction :: Evaluatable e => Env -> Id -> e -> (Val, Env)
internalFunction env f argsList = case evaledArgsList of
  List evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    evaledArgsList = fst $ evalIn env argsList
    fun "fold" (fun : init : List xs : _) =
      let foldFun acc x = fst $ evalIn env $ App (App (funToExpr fun) acc) x
       in foldl foldFun init xs
    fun "zipWith" (fun : List xs : List ys : _) =
      let zipFun :: Evaluatable e => e -> e -> Val
          zipFun x y = fst $ evalIn env $ App (App (funToExpr fun) x) y
       in List $ zipWith zipFun xs ys
    fun "head" xs = case xs of
      [] -> LNothing
      (x : _) -> LJust x
    fun "dictToList" (dict : _) = case dict of
      (Dictionary d) -> List $ map (\(k, v) -> (Tuple [k, v])) (Map.toList d)
    fun "sort" xs = List . List.sort $ xs
    fun x r = trace ("no such function" ++ show x ++ show r) $ error "No such function "

runFun :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (Function _ ids e3, env') ->
    let arg = head ids
        -- NOTE: move extension of 'whatever' to separate function
        newEnv = case arg of
          (Atom atomId) -> extend env' atomId e2
          (PTuple bindings) -> case (fst $ evalIn env' e2) of
            (Tuple vals) -> extendWithTuple env bindings vals
            _ -> error "Non-tuple value received for tuple destructuring"
        missingArgs' = missingArgs newEnv ids
     in if null missingArgs'
          then evalIn newEnv e3
          else evalIn newEnv (Lambda missingArgs' e3)
  (val, env) -> trace ("Cannot apply val: " ++ show val ++ "!") $ error ("Cannot apply value" ++ show env)

missingArgs :: Env -> [Expr] -> [Expr]
missingArgs env = filter fn
  where
    fn expr = isNothing $ traverse (inScope env) (atomIds expr)

atomIds :: Expr -> [String]
atomIds x = case x of
  Atom atomId -> [atomId]
  (PTuple atomList) -> concatMap atomIds atomList
  _ -> error "Non-atom function argument"

eval :: Evaluatable e => e -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Evaluatable e => Env -> e -> (Val, Env)
evalInEnv = evalIn

evals :: Evaluatable e => [e] -> Val
evals exprs = fst $ foldl fl (Undefined, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
