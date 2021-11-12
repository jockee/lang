{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval (Val (..), eval) where

import Data.Map qualified as Map
import Debug.Trace
import Syntax

data Val
  = FunVal Env [Id] Expr
  | BoolVal Bool
  | IntVal Integer
  | FloatVal Float

instance Show Val where
  show FunVal {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (BoolVal n) = show n

type Env = Id -> Val

extend :: Env -> [Id] -> Val -> Env
extend e x v y = if (head x) == y then v else e y

empty :: Env
empty _ = error "Not found!"

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
  _ == _ = False

instance Arith Val where
  evalOp Add = (+)
  evalOp Sub = (-)
  evalOp Mul = (*)
  evalOp Eql = \a b -> BoolVal $ a == b
  evalOp And = \a b -> case (a, b) of
    ((BoolVal a), (BoolVal b)) -> BoolVal $ a && b
    otherwise -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    ((BoolVal a), (BoolVal b)) -> BoolVal $ a || b
    otherwise -> BoolVal False

evalIn :: Env -> Expr -> Val
evalIn env (If (LBool True) c _) = evalIn env c
evalIn env (If (LBool False) _ a) = evalIn env a
evalIn env (Lambda ids e) = FunVal env ids e
evalIn env (App e1 e2) = runFun env e1 e2
evalIn env (Binop Pipe e1 e2) = runFun env e2 e1
-- evalIn env (Binop Assign k v) = evalIn (extend env k v)
evalIn env (Atom x) = env x
evalIn _ (LFloat n) = FloatVal n
evalIn _ (LInteger n) = IntVal n
evalIn _ (LBool n) = BoolVal n
evalIn env (Binop op e1 e2) =
  let v1 = evalIn env e1
      v2 = evalIn env e2
      x = evalOp op
   in v1 `x` v2
evalIn _ a = trace ("failed to find match in evalIn" ++ show a) $ IntVal 99

runFun env e1 e2 = case evalIn env e1 of
  FunVal env' xs e3 ->
    let v2 = evalIn env e2
     in evalIn (extend env' xs v2) e3
  _ -> error "Cannot apply value"

eval :: Expr -> Val
eval = evalIn empty
