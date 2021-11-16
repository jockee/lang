{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Syntax where

import Data.Data
import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql | NotEql | And | Or | Pipe | Assign | Concat
  deriving (Show, Data)

data Expr
  = Atom String
  | LList [Expr]
  | Dict [(Expr, Expr)]
  | DictUpdate Expr Expr
  | DictAccess Expr Expr
  | DictKey String
  | LInteger Integer
  | LFloat Float
  | LJust Expr
  | LNothing
  | LString String
  | LBool Bool
  | LFold Expr Expr Expr
  | If Expr Expr Expr
  | Lambda [Id] Expr
  | App Expr Expr
  | Binop Op Expr Expr
  | Cmp String Expr Expr
  | PNoop
  deriving (Data)

instance Show Expr where show = showExpr

showExpr :: Expr -> String
showExpr (LString contents) = "(LString \"" ++ contents ++ "\")"
showExpr (Atom name) = "(Atom \"" ++ name ++ "\")"
showExpr (LInteger contents) = "(LInteger " ++ show contents ++ ")"
showExpr (LFloat contents) = "(LFloat " ++ show contents ++ ")"
showExpr (LBool True) = "(LBool True)"
showExpr (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ show a ++ " " ++ show b ++ ")"
showExpr PNoop = "(Noop)"
showExpr (LBool False) = "(LBool False)"
showExpr (Dict pairs) = "(Dict [" ++ showDictContents pairs ++ "])"
showExpr (DictUpdate dict update) = "(DictUpdate " ++ showExpr dict ++ " " ++ showExpr update ++ ")"
showExpr (LNothing) = "(LNothing)"
showExpr (LJust e) = "(LJust " ++ show e ++ ")"
showExpr (LList contents) = "(LList [" ++ intercalate ", " (map show contents) ++ "])"
showExpr (If cond e1 e2) = "(If " ++ show cond ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
showExpr (LFold f i xs) = "(LFold " ++ showExpr f ++ showExpr i ++ showExpr xs ++ ")"
showExpr (App e1 e2) = "(App " ++ showExpr e1 ++ showExpr e2 ++ ")"
showExpr (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ showExpr e ++ ")"
showExpr (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showExpr _ = "UNKNOWN"

showDictContents pairs = intercalate ", " (map show pairs)
