{-# LANGUAGE GADTs #-}

module Syntax where

import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql | And | Or | Pipe | Assign
  deriving (Show)

data Expr
  = Atom String
  | List [Expr]
  | LInteger Integer
  | LFloat Float
  | LString String
  | LBool Bool
  | LMap Expr Expr -- FIXME: express map in terms of fold in stdlib
  | LConcat Expr Expr -- FIXME: express map in terms of fold in stdlib
  | LFold Expr Expr Expr
  | If Expr Expr Expr
  | Lambda [Id] Expr
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

pipeExpr :: Expr -> Expr -> Expr
pipeExpr = Binop Pipe

assignExpr :: Expr -> Expr -> Expr
assignExpr = Binop Assign

instance Show Expr where show = showVal

showVal :: Expr -> String
showVal (LString contents) = "(LString \"" ++ contents ++ "\")"
showVal (Atom name) = "(Atom \"" ++ name ++ "\")"
showVal (LInteger contents) = "(LInteger " ++ show contents ++ ")"
showVal (LFloat contents) = "(LFloat " ++ show contents ++ ")"
showVal (LBool True) = "(LBool True)"
showVal (LBool False) = "(LBool False)"
showVal (List contents) = "(List [" ++ intercalate ", " (map show contents) ++ "])"
showVal (If cond e1 e2) = "(If " ++ show cond ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
showVal (LConcat l1 l2) = "(LConcat " ++ showVal l1 ++ showVal l2 ++ ")"
showVal (LMap f xs) = "(LMap " ++ showVal f ++ showVal xs ++ ")"
showVal (LFold f i xs) = "(LFold " ++ showVal f ++ showVal i ++ showVal xs ++ ")"
showVal (App e1 e2) = "(App " ++ showVal e1 ++ showVal e2 ++ ")"
showVal (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ showVal e ++ ")"
showVal (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showVal _ = "UNKNOWN"
