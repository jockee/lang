{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval (Val (..), eval, evals, evalInEnv, Env, emptyEnv) where

import Control.Exception
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
import Syntax

data Val
  = FunVal Env [Id] Expr
  | BoolVal Bool
  | StringVal String
  | IntVal Integer
  | DictVal (Map.Map Val Val)
  | FloatVal Float
  | DictKeyVal String
  | Undefined
  | ListVal [Val]

instance Show Val where
  show FunVal {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (ListVal ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (DictVal m) = "{" ++ List.intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (DictKeyVal n) = n
  show (StringVal n) = show n
  show (BoolVal n) = show n

type Env = (Map.Map String Val, Map.Map String Expr)

emptyEnv :: (Map.Map String Val, Map.Map String Expr)
emptyEnv = (Map.empty, Map.empty)

extend :: Env -> [Id] -> Expr -> Env
extend env xs ex =
  ( Map.insert (head xs) (fst $ evalIn env ex) (fst env),
    Map.insert (head xs) ex (snd env)
  )

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
  (BoolVal True) == (BoolVal True) = True
  (BoolVal False) == (BoolVal False) = True
  (ListVal xs) == (ListVal ys) = xs == ys
  (DictVal m1) == (DictVal m2) = m1 == m2
  (DictKeyVal a) == (DictKeyVal b) = a == b
  (FunVal (valEnv1, exprEnv1) ids1 e1) == (FunVal (valEnv2, exprEnv2) ids2 e2) = valEnv1 == valEnv2 -- XXX: 1. currently only looks at vals, not exprs. 2. for testing purposes. lambda function equality is probably not very useful
  _ == _ = False

instance Arith Val where
  cmpOp ">" = \a b -> BoolVal $ a > b
  cmpOp "<" = \a b -> BoolVal $ a < b
  cmpOp ">=" = \a b -> BoolVal $ a >= b
  cmpOp "<=" = \a b -> BoolVal $ a <= b
  evalOp Add = (+)
  evalOp Sub = (-)
  evalOp Mul = (*)
  evalOp Eql = \a b -> BoolVal $ a == b
  evalOp NotEql = \a b -> BoolVal $ a /= b
  evalOp And = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a && b
    _ -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a || b
    _ -> BoolVal False

evalIn :: Env -> Expr -> (Val, Env)
evalIn env (If (LBool True) t _) = evalIn env t
evalIn env (If (LBool False) _ f) = evalIn env f
evalIn env (If condition ifTrue ifFalse) =
  let (val, env') = evalIn env condition
   in if val == BoolVal True then evalIn env' ifTrue else evalIn env' ifFalse
evalIn env (Lambda ids e) = (FunVal env ids e, env)
evalIn env (LFold f initExpr (Atom a)) = doFold env f initExpr (atomToExpr env a)
evalIn env (LFold f initExpr (List listExprs)) = doFold env f initExpr listExprs
evalIn env (App e1 e2) = trace ("VALENV: " ++ show (fst env) ++ "; " ++ show e1) $ runFun env e1 e2
evalIn env (Binop Concat e1 e2) =
  let (ListVal xs, _) = evalIn env e1
      (ListVal ys, _) = evalIn env e2
   in (ListVal $ xs ++ ys, env)
evalIn env (Binop Pipe e1 e2) = runFun env e2 e1
evalIn env (Binop Assign (Atom a) v) =
  let env'' = extend env' [a] v
      (value, env') = evalIn env v
   in (value, env'')
evalIn env (Atom x) = case Map.lookup x $ fst env of
  Nothing -> throw $ EvalException $ "Atom " ++ x ++ " does not exist in env"
  Just a -> (a, env)
evalIn env (LString n) = (StringVal n, env)
evalIn env (LFloat n) = (FloatVal n, env)
evalIn env (LInteger n) = (IntVal n, env)
evalIn env (DictUpdate baseDict updateDict) =
  let (DictVal d1) = fst $ evalIn env baseDict
      (DictVal d2) = fst $ evalIn env updateDict
   in (DictVal $ Map.unionWith (+) d1 d2, env)
evalIn env (DictAccess k dict) =
  let (DictVal m) = fst $ evalIn env dict
      kv = fst $ evalIn env k
   in (fromJust (Map.lookup kv m), env)
evalIn env (DictKey k) = (DictKeyVal k, env)
evalIn env (Dict pairs) =
  let fn (k, v) = (fst $ evalIn env k, fst $ evalIn env v)
   in (DictVal $ Map.fromList $ map fn pairs, env)
evalIn env (List es) = (ListVal $ map (fst . evalIn env) es, env)
evalIn env (LBool n) = (BoolVal n, env)
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
evalIn env Noop = (Undefined, env)
evalIn _ a = trace ("failed to find match in evalIn" ++ show a) undefined

doFold :: Foldable t => Env -> Expr -> Expr -> t Expr -> (Val, Env)
doFold env f initExpr listExprs = evalIn env $ foldl foldFun initExpr listExprs
  where
    foldFun acc x = App (App f acc) x

runFun :: Env -> Expr -> Expr -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (FunVal _ xs e3, env') ->
    let env'' = extend env' xs e2
        missingArgs = filter (\x -> not $ Map.member x $ fst env'') xs
     in if null missingArgs
          then evalIn env'' e3
          else evalIn env'' (Lambda missingArgs e3)
  val -> trace ("cannot apply val: " ++ show val) error "Cannot apply value"

atomToExpr :: Env -> String -> [Expr]
atomToExpr env atomId = case Map.lookup atomId (snd env) of
  Nothing -> throw $ EvalException "Can't traverse non-list"
  Just (List listExprs) -> listExprs

eval :: Expr -> Val
eval = fst . evalInEnv emptyEnv

evalInEnv :: Env -> Expr -> (Val, Env)
evalInEnv = evalIn

evals :: [Expr] -> Val
evals exprs = fst $ foldl fl (Undefined, emptyEnv) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
