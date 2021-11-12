module Syntax where

import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql deriving (Eq, Show)

data Expr
  = Atom String
  | List [Expr]
  | Integer Integer
  | Float Float
  | String String
  | Bool Bool
  | If Expr Expr Expr
  | Abs Id Expr
  | App Expr Expr
  | Var Id
  | Binop Op Expr Expr
  deriving (Eq)

source :: Expr -> String
source expr = case expr of
  (Abs x e) -> parens $ "\\" ++ x ++ " -> " ++ source e
  (App e1 e2) -> parens $ source e1 ++ " " ++ source e2
  (Binop op e1 e2) -> parens $ source e1 ++ sourceOp op ++ source e2
  (Var x) -> x
  (Integer n) -> show n
  where
    sourceOp Add = " + "
    sourceOp Sub = " - "
    sourceOp Mul = " * "
    parens s = "(" ++ s ++ ")"

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul

eqExpr :: Expr -> Expr -> Expr
eqExpr = Binop Eql

instance Show Expr where show = showVal

showVal :: Expr -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name ++ "(Atom)"
showVal (Integer contents) = show contents ++ "(Int)"
showVal (Float contents) = show contents ++ "(Float)"
showVal (Bool True) = "true(Bool)"
showVal (Bool False) = "false(Bool)"
showVal (List contents) = "[" ++ unwordsList contents ++ "]"
showVal (If cond e1 e2) = "if "
showVal (Var sid) = "(var" ++ sid ++ ")"
showVal (App e1 e2) = "(App " ++ showVal e1 ++ showVal e2
showVal (Abs id e) = "(Abs " ++ id ++ ": " ++ showVal e
showVal (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " & " ++ show d ++ ")"
showVal _ = "UNKNOWN"

unwordsList :: [Expr] -> String
unwordsList = intercalate "" . intersperse ", " . map showVal
