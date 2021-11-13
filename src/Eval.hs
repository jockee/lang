{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval (Val (..), eval, evals, evalInEnv, Env) where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Syntax

data Val
  = FunVal Env [Id] Expr
  | BoolVal Bool
  | StringVal String
  | IntVal Integer
  | FloatVal Float
  | Undefined
  | ListVal [Val]

instance Show Val where
  show FunVal {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (ListVal ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (StringVal n) = show n
  show (BoolVal n) = show n

type Env = Map.Map String Val

extend :: Env -> [Id] -> Val -> Env
extend env xs v = Map.insert (head xs) v env

class Num a => Arith a where
  evalOp :: Op -> (a -> a -> Val)

instance Num Val where
  (FloatVal i1) + (FloatVal i2) = FloatVal (i1 + i2)
  (IntVal i1) + (IntVal i2) = IntVal (i1 + i2)
  (FloatVal i1) * (FloatVal i2) = FloatVal (i1 * i2)
  (IntVal i1) * (IntVal i2) = IntVal (i1 * i2)
  (FloatVal i1) - (FloatVal i2) = FloatVal (i1 - i2)
  (IntVal i1) - (IntVal i2) = IntVal (i1 - i2)

instance Eq Val where
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (BoolVal True) == (BoolVal True) = True
  (BoolVal False) == (BoolVal False) = True
  (ListVal xs) == (ListVal ys) = xs == ys
  (FunVal env1 ids1 e1) == (FunVal env2 ids2 e2) = env1 == env2 -- for testing purposes. lambda function equality is probably not very useful
  _ == _ = False

instance Arith Val where
  evalOp Add = (+)
  evalOp Sub = (-)
  evalOp Mul = (*)
  evalOp Eql = \a b -> BoolVal $ a == b
  evalOp And = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a && b
    _ -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a || b
    _ -> BoolVal False

evalIn :: Env -> Expr -> (Val, Env)
evalIn env (If (LBool True) c _) = evalIn env c
evalIn env (If (LBool False) _ a) = evalIn env a
evalIn env (Lambda ids e) = (FunVal env ids e, env)
evalIn env (LConcat e1 e2) =
  let (ListVal xs, _) = evalIn env e1
      (ListVal ys, _) = evalIn env e2
   in (ListVal $ xs ++ ys, env)
evalIn env (LMap f (List xs)) = (ListVal $ map (fst . evalIn env . App f) xs, env)
evalIn env (LFold f initExpr (List listExprs)) =
  let foldFun :: Expr -> Expr -> Expr
      foldFun acc x = App (App f acc) x
   in evalIn env $ foldl foldFun initExpr listExprs
evalIn env (App e1 e2) = runFun env e1 e2
evalIn env (Binop Pipe e1 e2) = runFun env e2 e1
evalIn env (Binop Assign (Atom a) v) =
  let (value, env') = evalIn env v
      env'' = extend env' [a] value
   in (value, env'')
evalIn env (Atom x) = (fromJust $ Map.lookup x env, env)
evalIn env (LString n) = (StringVal n, env)
evalIn env (LFloat n) = (FloatVal n, env)
evalIn env (LInteger n) = (IntVal n, env)
evalIn env (List es) = (ListVal $ map (fst . evalIn env) es, env)
evalIn env (LBool n) = (BoolVal n, env)
evalIn env (Binop op e1 e2) =
  let (v1, _) = evalIn env e1
      (v2, _) = evalIn env e2
      x = evalOp op
   in (v1 `x` v2, env)
evalIn _ a = trace ("failed to find match in evalIn" ++ show a) undefined

appVal :: Env -> Expr -> (Val, Env)
appVal env (App e1 v) = (IntVal 9, env)

runFun :: Env -> Expr -> Expr -> (Val, Env)
runFun env e1 e2 = case evalIn env e1 of
  (FunVal env' xs e3, _) ->
    let v2 = evalIn env e2
        env'' = extend env' xs (fst v2)
        missingArgs = filter (\x -> not $ Map.member x env'') xs
     in if null missingArgs
          then evalIn env'' e3
          else evalIn env'' (Lambda missingArgs e3)
  val -> trace ("cannot apply val: " ++ show val) error "Cannot apply value"

eval :: Expr -> Val
eval = fst . evalInEnv Map.empty

evalInEnv :: Env -> Expr -> (Val, Env)
evalInEnv = evalIn

evals :: [Expr] -> Val
evals exprs = fst $ foldl fl (Undefined, Map.empty) exprs
  where
    fl (_val, env) ex = evalInEnv env ex
