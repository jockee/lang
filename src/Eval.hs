{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval (Val (..), eval, evals, evalInEnv, Env (..), emptyEnv, resetScope) where

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

data Val
  = Function Env [Id] Expr
  | Boolean Bool
  | StringVal String
  | IntVal Integer
  | Dictionary (Map.Map Val Val)
  | FloatVal Float
  | DictKeyVal String
  | Noop
  | List [Val]
  deriving (Data)

instance Show Val where
  show Function {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (List ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (Dictionary m) = "{" ++ List.intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (DictKeyVal n) = n
  show (StringVal n) = show n
  show (Boolean n) = show n

data Env = Env
  { envValues :: Map.Map String Val,
    envExpressions :: Map.Map String Expr,
    envScopes :: [String]
  }
  deriving (Show, Data)

defaultEnvScopes = ["global"]

emptyEnv :: Env
emptyEnv = Env {envValues = Map.empty, envExpressions = Map.empty, envScopes = defaultEnvScopes}

extend :: Env -> [Id] -> Expr -> Env
extend env xs ex =
  env
    { envValues = Map.insert key (fst $ evalIn env ex) (envValues env),
      envExpressions = Map.insert key ex (envExpressions env)
    }
  where
    key = (last $ envScopes env) ++ ":" ++ head xs

resetScope :: Env -> Env
resetScope env = env {envScopes = defaultEnvScopes}

withScope :: Env -> Env
withScope env = newEnv
  where
    newScope = hash newEnv
    newEnv = env {envScopes = List.nub $ envScopes env ++ [show newScope]}

-- XXX: merge/generalize inScopeE/inScopeV
inScopeE :: Env -> String -> Maybe Expr
inScopeE env atomId = asum $ map (\k -> Map.lookup k (envExpressions env)) scopeKeys
  where
    scopeKeys = map (\k -> k ++ ":" ++ atomId) $ reverse $ envScopes env

inScopeV :: Env -> String -> Maybe Val
inScopeV env atomId = asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = map (\k -> k ++ ":" ++ atomId) $ reverse $ envScopes env

class Num a => Arith a where
  cmpOp :: String -> (a -> a -> Val)
  evalOp :: Op -> (a -> a -> Val)

instance Num Val where
  (FloatVal i1) + (FloatVal i2) = FloatVal (i1 + i2)
  (IntVal i1) + (IntVal i2) = IntVal (i1 + i2)
  (FloatVal i1) * (FloatVal i2) = FloatVal (i1 * i2)
  (IntVal i1) * (IntVal i2) = IntVal (i1 * i2)
  (FloatVal i1) - (FloatVal i2) = FloatVal (i1 - i2)
  (IntVal i1) - (IntVal i2) = IntVal (i1 - i2)

instance Ord Val where
  compare (DictKeyVal i1) (DictKeyVal i2) = compare i1 i2
  compare (FloatVal i1) (FloatVal i2) = compare i1 i2
  compare (IntVal i1) (IntVal i2) = compare i1 i2

instance Eq Val where
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (Boolean True) == (Boolean True) = True
  (Boolean False) == (Boolean False) = True
  (List xs) == (List ys) = xs == ys
  (Dictionary m1) == (Dictionary m2) = m1 == m2
  (DictKeyVal a) == (DictKeyVal b) = a == b
  (Function env1 ids1 e1) == (Function env2 ids2 e2) = envValues env1 == envValues env2 -- XXX: 1. currently only looks at vals, not exprs. 2. for testing purposes. lambda function equality is probably not very useful
  _ == _ = False

instance Arith Val where
  cmpOp ">" = \a b -> Boolean $ a > b
  cmpOp "<" = \a b -> Boolean $ a < b
  cmpOp ">=" = \a b -> Boolean $ a >= b
  cmpOp "<=" = \a b -> Boolean $ a <= b
  evalOp Add = (+)
  evalOp Sub = (-)
  evalOp Mul = (*)
  evalOp Eql = \a b -> Boolean $ a == b
  evalOp NotEql = \a b -> Boolean $ a /= b
  evalOp And = \a b -> case (a, b) of
    (Boolean a, Boolean b) -> Boolean $ a && b
    _ -> Boolean False
  evalOp Or = \a b -> case (a, b) of
    (Boolean a, Boolean b) -> Boolean $ a || b
    _ -> Boolean False

evalIn :: Env -> Expr -> (Val, Env)
evalIn env (If (PBool True) t _) = evalIn env t
evalIn env (If (PBool False) _ f) = evalIn env f
evalIn env (If condition ifTrue ifFalse) =
  let (val, env') = evalIn env condition
   in if val == Boolean True then evalIn env' ifTrue else evalIn env' ifFalse
evalIn env (Lambda ids e) = (Function env ids e, env)
evalIn env (LFold f initExpr (Atom a)) = doFold env f initExpr (atomToExpr env a)
evalIn env (LFold f initExpr (PList listExprs)) = doFold env f initExpr listExprs
evalIn env (App e1 e2) = runFun (withScope env) e1 e2
evalIn env (Binop Concat e1 e2) =
  let (List xs, _) = evalIn env e1
      (List ys, _) = evalIn env e2
   in (List $ xs ++ ys, env)
evalIn env (Binop Pipe e1 e2) = runFun (withScope env) e2 e1
evalIn env (Binop Assign (Atom a) v) =
  let env'' = extend env' [a] v
      (value, env') = evalIn env v
   in (value, env'')
evalIn env (Atom atomId) = case inScopeV env atomId of
  Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
  Just a -> (a, env)
evalIn env (PString n) = (StringVal n, env)
evalIn env (PFloat n) = (FloatVal n, env)
evalIn env (PInteger n) = (IntVal n, env)
evalIn env (DictUpdate baseDict updateDict) =
  let (Dictionary d1) = fst $ evalIn env baseDict
      (Dictionary d2) = fst $ evalIn env updateDict
   in (Dictionary $ Map.union d2 d1, env)
evalIn env (DictAccess k dict) =
  let (Dictionary m) = fst $ evalIn env dict
      kv = fst $ evalIn env k
   in (fromJust (Map.lookup kv m), env)
evalIn env (DictKey k) = (DictKeyVal k, env)
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
evalIn env PNoop = (Noop, env)
evalIn _ a = trace ("failed to find match in evalIn" ++ show a) undefined

doFold :: Foldable t => Env -> Expr -> Expr -> t Expr -> (Val, Env)
doFold env f initExpr listExprs = evalIn env $ foldl foldFun initExpr listExprs
  where
    foldFun acc x = App (App f acc) x

runFun :: Env -> Expr -> Expr -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (Function _ xs e3, env') ->
    let env'' = extend env' xs e2
        missingArgs = filter (isNothing . inScopeV env'') xs
     in if null missingArgs
          then evalIn env'' e3
          else evalIn env'' (Lambda missingArgs e3)
  val -> trace ("cannot apply val: " ++ show val) error "Cannot apply value"

atomToExpr :: Env -> String -> [Expr]
atomToExpr env atomId = case inScopeE env atomId of
  Nothing -> throw $ EvalException "Can't traverse non-list"
  Just (PList listExprs) -> listExprs

eval :: Expr -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Env -> Expr -> (Val, Env)
evalInEnv = evalIn

evals :: [Expr] -> Val
evals exprs = fst $ foldl fl (Noop, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
