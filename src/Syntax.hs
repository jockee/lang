{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax where

import Data.Data
import Data.List as List
import Data.Map qualified as Map

data Env where
  Env ::
    { envValues :: Map.Map String Val,
      envScopes :: [String]
    } ->
    Env

instance Show Env where
  show e = "env"

class Show e => Evaluatable e where
  evalIn :: Env -> e -> (Val, Env)

type Id = String

data Op = Add | Sub | Mul | Eql | NotEql | And | Or | Pipe | Assign | Concat
  deriving (Show, Data)

data Expr where
  Atom :: String -> Expr
  PList :: [Expr] -> Expr
  PDict :: [(Expr, Expr)] -> Expr
  PDictUpdate :: Expr -> Expr -> Expr
  DictAccess :: Expr -> Expr -> Expr
  PDictKey :: String -> Expr
  PInteger :: Integer -> Expr
  PFloat :: Float -> Expr
  PJust :: Expr -> Expr
  PNothing :: Expr
  PString :: String -> Expr
  PBool :: Bool -> Expr
  PIf :: Expr -> Expr -> Expr -> Expr
  InternalFunction :: Id -> Expr -> Expr
  Lambda :: (Show e, Evaluatable e) => [Id] -> e -> Expr
  App :: (Show e, Evaluatable e) => Expr -> e -> Expr
  Binop :: Op -> Expr -> Expr -> Expr
  Cmp :: String -> Expr -> Expr -> Expr
  PNoop :: Expr
  deriving (Typeable)

data Val where
  Function :: (Show e, Evaluatable e) => Env -> [Id] -> e -> Val
  Boolean :: Bool -> Val
  StringVal :: String -> Val
  IntVal :: Integer -> Val
  Dictionary :: Map.Map Val Val -> Val
  FloatVal :: Float -> Val
  DictKey :: String -> Val
  Undefined :: Val
  LJust :: Val -> Val
  LNothing :: Val
  List :: [Val] -> Val
  deriving (Typeable)

instance Show Val where
  show Function {} = "<fun>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (List ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (Dictionary m) = "{" ++ List.intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (LJust v) = "Just " ++ show v
  show (LNothing) = "Nothing"
  show (DictKey n) = n
  show (StringVal n) = show n
  show (Boolean n) = show n

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
  compare (DictKey i1) (DictKey i2) = compare i1 i2
  compare (FloatVal i1) (FloatVal i2) = compare i1 i2
  compare (IntVal i1) (IntVal i2) = compare i1 i2

instance Eq Val where
  LNothing == LNothing = True
  LJust v1 == LJust v2 = v1 == v2
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (Boolean True) == (Boolean True) = True
  (Boolean False) == (Boolean False) = True
  (List xs) == (List ys) = xs == ys
  (Dictionary m1) == (Dictionary m2) = m1 == m2
  (DictKey a) == (DictKey b) = a == b
  (Function env1 ids1 e1) == (Function env2 ids2 e2) = envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful
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

instance Show Expr where show = showExpr

showExpr :: Expr -> String
showExpr (PString contents) = "(PString \"" ++ contents ++ "\")"
showExpr (Atom name) = "(Atom \"" ++ name ++ "\")"
showExpr (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showExpr (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showExpr (PBool True) = "(PBool True)"
showExpr (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ show a ++ " " ++ show b ++ ")"
showExpr PNoop = "(PNoop)"
showExpr (PBool False) = "(PBool False)"
showExpr (PDict pairs) = "(PDict [" ++ showDictContents pairs ++ "])"
showExpr (PDictUpdate dict update) = "(PDictUpdate " ++ showExpr dict ++ " " ++ showExpr update ++ ")"
showExpr (PList contents) = "(PList [" ++ intercalate ", " (map show contents) ++ "])"
showExpr (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ show argList ++ ")"
showExpr (PNothing) = "(PNothing)"
showExpr (PJust e) = "(PJust " ++ show e ++ ")"
showExpr (PIf cond e1 e2) = "(PIf " ++ show cond ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
showExpr (App e1 e2) = "(App " ++ show e1 ++ " " ++ show e2 ++ ")"
showExpr (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ show e ++ ")"
showExpr (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showExpr _ = "UNKNOWN"

showDictContents pairs = intercalate ", " (map show pairs)
