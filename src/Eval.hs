{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval (Val (..), eval) where

import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Syntax

data Val
  = FunVal Env [Id] Expr
  | BoolVal Bool
  | IntVal Integer
  | FloatVal Float
  | ListVal [Val]

instance Show Val where
  show FunVal {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (ListVal ns) = "[" ++ intercalate ", " (map show ns) ++ "]"
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
    otherwise -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a || b
    otherwise -> BoolVal False

evalIn :: Env -> Expr -> Val
evalIn env (If (LBool True) c _) = evalIn env c
evalIn env (If (LBool False) _ a) = evalIn env a
evalIn env (Lambda ids e) = FunVal env ids e
evalIn env (LMap f (List xs)) = ListVal $ map (evalIn env . App f) xs
evalIn env (App e1 e2) = runFun env e1 e2
evalIn env (Binop Pipe e1 e2) = runFun env e2 e1
evalIn env (Atom x) = fromJust $ Map.lookup x env
evalIn _ (LFloat n) = FloatVal n
evalIn _ (LInteger n) = IntVal n
evalIn _ (LBool n) = BoolVal n
evalIn env (Binop op e1 e2) =
  let v1 = evalIn env e1
      v2 = evalIn env e2
      x = evalOp op
   in v1 `x` v2
evalIn _ a = trace ("failed to find match in evalIn" ++ show a) $ IntVal 99

runFun :: Env -> Expr -> Expr -> Val
runFun env e1 e2 = case evalIn env e1 of
  FunVal env' xs e3 ->
    let v2 = evalIn env e2
        env'' = extend env' xs v2
        missingArgs = filter (\x -> not $ Map.member x env'') xs
     in if null missingArgs
          then evalIn env'' e3
          else evalIn env'' (Lambda missingArgs e3)
  _ -> error "Cannot apply value"

eval :: Expr -> Val
eval = evalIn Map.empty
