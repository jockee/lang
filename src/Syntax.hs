{-# LANGUAGE GADTs #-}

module Syntax where

import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql | And | Or
  deriving (Show)

data Expr
  = Atom String
  | List [Expr]
  | LInteger Integer
  | LFloat Float
  | LString String
  | LBool Bool
  | If Expr Expr Expr
  | Abs [Id] Expr
  | App Expr Expr
  | Binop Op Expr Expr

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul

eqExpr :: Expr -> Expr -> Expr
eqExpr = Binop Eql

andExpr :: Expr -> Expr -> Expr
andExpr = Binop And

orExpr :: Expr -> Expr -> Expr
orExpr = Binop Or

instance Show Expr where show = showVal

showVal :: Expr -> String
showVal (LString contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name ++ "(Atom)"
showVal (LInteger contents) = show contents ++ "(Int)"
showVal (LFloat contents) = show contents ++ "(Float)"
showVal (LBool True) = "true(Bool)"
showVal (LBool False) = "false(Bool)"
showVal (List contents) = "[" ++ unwordsList contents ++ "]"
showVal (If cond e1 e2) = "if "
showVal (App e1 e2) = "(App " ++ showVal e1 ++ showVal e2
showVal (Abs ids e) = "(Abs " ++ show (intercalate "," ids) ++ ": " ++ showVal e
showVal (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " & " ++ show d ++ ")"
showVal _ = "UNKNOWN"

unwordsList :: [Expr] -> String
unwordsList = intercalate "" . intersperse ", " . map showVal
