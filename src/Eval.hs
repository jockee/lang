{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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

-- deriving (Data)

-- data Val
--   = Function Env [Id] Expr
--   | Boolean Bool
--   | StringVal String
--   | IntVal Integer
--   | Dictionary (Map.Map Val Val)
--   | FloatVal Float
--   | DictKey String
--   | Undefined
--   | LJust Val
--   | LNothing
--   | List [Val]
--   deriving (Data)

-- deriving (Show, Data)

defaultEnvScopes :: [String]
defaultEnvScopes = ["global"]

emptyEnv :: Env
emptyEnv = Env {envValues = Map.empty, envScopes = defaultEnvScopes}

extend :: Evaluatable e => Env -> [Id] -> e -> Env
extend env xs ex =
  Env
    { envValues = Map.insert key (fst $ evalIn env ex) (envValues env),
      envScopes = envScopes env
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

inScope :: Env -> String -> Maybe Val
inScope env atomId = asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = map (\k -> k ++ ":" ++ atomId) $ reverse $ envScopes env

-- doFold :: Foldable t => Env -> Expr -> Expr -> t Expr -> (Val, Env)
-- doFold env f initExpr listExprs = evalIn env $ foldl foldFun initExpr listExprs
--   where
--     foldFun acc x = App (App f acc) x

doFoldVal :: (Show e, Evaluatable e) => Env -> e -> e -> e -> (Val, Env)
doFoldVal env f initExpr listExprs = trace ("foldFun" ++ show f) $ (foldl foldFun initVal listVals, env)
  where
    initVal :: Val
    initVal = fst $ evalIn env initExpr
    listVals :: [Val]
    listVals = case fst $ evalIn env listExprs of
      List xs -> xs
      val -> trace ("fold got non-list" ++ show val) error "Fold got non-list"
    foldFun :: Val -> Val -> Val
    foldFun acc x = fst $ evalIn env f

-- should take already calculated vals and apply function to it

internalFunction :: Evaluatable e => Env -> Id -> [e] -> (Val, Env)
internalFunction env f argsList = (fun f evaledArgsList, env)
  where
    evaledArgsList = map (fst . evalIn env) argsList
    fun "head" (List xs : _) = case xs of
      [] -> LNothing
      (x : _) -> LJust x
    fun "sort" (List xs : _) = List . List.sort $ xs

runFun :: (Show e, Evaluatable e) => Env -> e -> e -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (Function _ xs e3, env') ->
    let env'' = extend env' xs e2
        missingArgs = filter (isNothing . inScope env'') xs
     in if null missingArgs
          then evalIn env'' e3 -- could be 'x + 1'
          else (IntVal 1, env)
  -- XXX: evalIn env'' (Lambda missingArgs e3)
  val -> trace ("cannot apply val: " ++ show val) error "Cannot apply value"

-- -- XXX: hopefully obsolete soon
-- atomToExpr :: Env -> String -> [Expr]
-- atomToExpr env atomId = case inScopeE env atomId of
--   Nothing -> throw $ EvalException "Can't traverse non-list"
--   Just (PList listExprs) -> listExprs
--   Just x -> trace ("calling f with x = " ++ show x) $ [x]

eval :: Evaluatable e => e -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Evaluatable e => Env -> e -> (Val, Env)
evalInEnv = evalIn

evals :: Evaluatable e => [e] -> Val
evals exprs = fst $ foldl fl (Undefined, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
