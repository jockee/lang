{-# LANGUAGE GADTs #-}

module Syntax where

import Data.List

type Id = String

data Op = Add | Sub | Mul | Eql | And | Or | Pipe | Assign | Concat
  deriving (Show)

data Expr
  = Atom String
  | List [Expr]
  | LInteger Integer
  | LFloat Float
  | LString String
  | LBool Bool
  | LMap Expr Expr -- FIXME: express map in terms of fold in stdlib
  | LFold Expr Expr Expr
  | If Expr Expr Expr
  | Lambda [Id] Expr
  | App Expr Expr
  | Binop Op Expr Expr

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
showVal (LMap f xs) = "(LMap " ++ showVal f ++ showVal xs ++ ")"
showVal (LFold f i xs) = "(LFold " ++ showVal f ++ showVal i ++ showVal xs ++ ")"
showVal (App e1 e2) = "(App " ++ showVal e1 ++ showVal e2 ++ ")"
showVal (Lambda ids e) = "(Lambda [\"" ++ (intercalate "\", \"" ids) ++ "\"] " ++ showVal e ++ ")"
showVal (Binop t s d) = "(Binop " ++ show t ++ " " ++ show s ++ " " ++ show d ++ ")"
showVal _ = "UNKNOWN"
