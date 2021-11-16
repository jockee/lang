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
  evalIn env (Binop Concat e1 e2) =
    let (List xs, _) = trace ("calling f with x = " ++ show (evalIn env e1)) $ evalIn env e1
        (List ys, _) = evalIn env e2
        e = error "Invalid"
     in (List $ xs ++ ys, env)
  evalIn env (Binop Pipe e1 e2) = runFun (withScope env) e2 e1
  evalIn env (Binop Assign (Atom a) v) =
    let env'' = extend env' a v
        (value, env') = evalIn env v
     in (value, env'')
  evalIn env (Atom atomId) = case inScope env atomId of
    Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
    Just a -> (a, env)
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
  evalIn env (PList es) = (List $ map (fst . evalIn env) es, env)
  evalIn env (PBool n) = (Boolean n, env)
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

extend :: Evaluatable e => Env -> Id -> e -> Env
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
    newScope = hash newEnv
    newEnv = env {envScopes = List.nub $ envScopes env ++ [show newScope]}

inScope :: Env -> String -> Maybe Val
inScope env atomId = asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = map (\k -> k ++ ":" ++ atomId) $ reverse $ envScopes env

internalFunction :: Evaluatable e => Env -> Id -> e -> (Val, Env)
internalFunction env f argsList = case evaledArgsList of
  List evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    evaledArgsList = fst $ evalIn env argsList
    fun "foldy" (fun : init : List xs : _) =
      let foldFun :: Evaluatable e => e -> e -> Val
          foldFun acc x = fst $ evalIn env $ App (App (funToExpr fun) acc) x
       in foldl foldFun init xs
    fun "head" xs = case xs of
      [] -> LNothing
      (x : _) -> LJust x
    fun "sort" xs = List . List.sort $ xs
    fun x r = trace ("no such function" ++ show x ++ show r) $ error "No such function "

runFun :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (Function _ xs e3, env') ->
    let env'' = extend env' (head xs) e2
        missingArgs = filter (isNothing . inScope env'') xs
     in if null missingArgs
          then evalIn env'' e3
          else evalIn env'' (Lambda missingArgs e3)
  val -> trace ("cannot apply val: " ++ show val) error "Cannot apply value"

eval :: Evaluatable e => e -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Evaluatable e => Env -> e -> (Val, Env)
evalInEnv = evalIn

evals :: Evaluatable e => [e] -> Val
evals exprs = fst $ foldl fl (Undefined, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
