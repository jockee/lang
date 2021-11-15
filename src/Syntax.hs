{-# LANGUAGE GADTs #-}

module Syntax where

import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql | NotEql | And | Or | Pipe | Assign | Concat
  deriving (Show)

data Expr
  = Atom String
  | List [Expr]
  | Dict [(Expr, Expr)]
  | DictAccess Expr Expr
  | DictKey String
  | LInteger Integer
  | LFloat Float
  | LString String
  | LBool Bool
  | LFold Expr Expr Expr
  | If Expr Expr Expr
  | Lambda [Id] Expr
  | App Expr Expr
  | Binop Op Expr Expr
  | Cmp String Expr Expr
  | Noop

instance Show Expr where show = showVal

showVal :: Expr -> String
showVal (LString contents) = "(LString \"" ++ contents ++ "\")"
showVal (Atom name) = "(Atom \"" ++ name ++ "\")"
showVal (LInteger contents) = "(LInteger " ++ show contents ++ ")"
showVal (LFloat contents) = "(LFloat " ++ show contents ++ ")"
showVal (LBool True) = "(LBool True)"
showVal (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ show a ++ " " ++ show b ++ ")"
showVal (Noop) = "(Noop)"
showVal (LBool False) = "(LBool False)"
showVal (Dict pairs) = "(Dict [" ++ intercalate ", " (map show pairs) ++ "])"
showVal (DictKey k) = "(DictKey \"" ++ show k ++ "\")"
showVal (List contents) = "(List [" ++ intercalate ", " (map show contents) ++ "])"
showVal (If cond e1 e2) = "(If " ++ show cond ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
showVal (LFold f i xs) = "(LFold " ++ showVal f ++ showVal i ++ showVal xs ++ ")"
showVal (App e1 e2) = "(App " ++ showVal e1 ++ showVal e2 ++ ")"
showVal (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ showVal e ++ ")"
showVal (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showVal _ = "UNKNOWN"
