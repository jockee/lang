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
  | PList [Expr]
  | PDict [(Expr, Expr)]
  | DictUpdate Expr Expr
  | DictAccess Expr Expr
  | DictKey String
  | PInteger Integer
  | PFloat Float
  | PJust Expr
  | PNothing
  | PString String
  | PBool Bool
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
showExpr (PString contents) = "(PString \"" ++ contents ++ "\")"
showExpr (Atom name) = "(Atom \"" ++ name ++ "\")"
showExpr (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showExpr (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showExpr (PBool True) = "(PBool True)"
showExpr (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ show a ++ " " ++ show b ++ ")"
showExpr PNoop = "(Noop)"
showExpr (PBool False) = "(PBool False)"
showExpr (PDict pairs) = "(PDict [" ++ showDictContents pairs ++ "])"
showExpr (DictUpdate dict update) = "(DictUpdate " ++ showExpr dict ++ " " ++ showExpr update ++ ")"
showExpr (PNothing) = "(PNothing)"
showExpr (PJust e) = "(PJust " ++ show e ++ ")"
showExpr (PList contents) = "(PList [" ++ intercalate ", " (map show contents) ++ "])"
showExpr (If cond e1 e2) = "(If " ++ show cond ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
showExpr (LFold f i xs) = "(LFold " ++ showExpr f ++ showExpr i ++ showExpr xs ++ ")"
showExpr (App e1 e2) = "(App " ++ showExpr e1 ++ showExpr e2 ++ ")"
showExpr (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ showExpr e ++ ")"
showExpr (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showExpr _ = "UNKNOWN"

showDictContents pairs = intercalate ", " (map show pairs)
