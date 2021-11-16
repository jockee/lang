{-# LANGUAGE TypeFamilies #-}

module Evaluatable where

import Control.Exception
import Data.Data
import Data.Foldable (asum)
import Data.Hashable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Eval
import Exceptions
import Syntax

instance Evaluatable Val where
  evalIn env val = (val, env)

instance Evaluatable Expr where
  evalIn env (PIf (PBool True) t _) = evalIn env t
  evalIn env (PIf (PBool False) _ f) = evalIn env f
  evalIn env (PIf condition ifTrue ifFalse) =
    let (val, env') = evalIn env condition
     in if val == Boolean True then evalIn env' ifTrue else evalIn env' ifFalse
  evalIn env (Lambda ids e) = (Function env ids e, env)
  evalIn env (PFold f initExpr listExpr) = doFoldVal env f initExpr listExpr
  -- evalIn env (PFold f initExpr (PList a)) = doFold env f initExpr a
  -- evalIn env (PFold f initExpr (Atom a)) = doFold env f initExpr (atomToExpr env a)
  evalIn env (InternalFunction f args) = internalFunction env f args
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
  evalIn _ a = trace ("failed to find match in evalIn" ++ show a) undefined
