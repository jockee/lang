{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax where

import Data.Data
import Data.List as List
import Data.Map qualified as Map

-- ENV

data Env where
  Env ::
    { envValues :: Map.Map String [Val],
      envScopes :: [String],
      typeSigs :: Map.Map String TypeSig
    } ->
    Env

instance Show Env where
  show Env {typeSigs = t, envValues = v, envScopes = s} = show v ++ show s ++ show t

instance Eq Env where
  Env {typeSigs = t1, envValues = v1, envScopes = s1} == Env {typeSigs = t2, envValues = v2, envScopes = s2} = t1 == t2 && v1 == v2 && s1 == s2

-- Evaluatable
class Show e => Evaluatable e where
  evalIn :: Env -> e -> (Val, Env)
  toExpr :: e -> Expr

-- Expr

data Expr where
  Atom :: TypeSig -> String -> Expr
  PTuple :: TypeSig -> [Expr] -> Expr
  PList :: TypeSig -> [Expr] -> Expr
  PDict :: TypeSig -> [(Expr, Expr)] -> Expr
  PDictUpdate :: Expr -> Expr -> Expr
  DictAccess :: Expr -> Expr -> Expr
  PDictKey :: String -> Expr
  PInteger :: Integer -> Expr
  PFloat :: Float -> Expr
  PJust :: TypeSig -> Expr -> Expr
  PNothing :: Expr
  PString :: String -> Expr
  PBool :: Bool -> Expr
  PIf :: Expr -> Expr -> Expr -> Expr
  InternalFunction :: Id -> Expr -> Expr
  Lambda :: (Show e, Evaluatable e) => TypeSig -> [Expr] -> e -> Expr
  App :: (Show e, Evaluatable e) => Expr -> e -> Expr
  Binop :: Op -> Expr -> Expr -> Expr
  Cmp :: String -> Expr -> Expr -> Expr
  NamedTypeSig :: TypeSig -> Expr
  PRange :: TypeSig -> Expr -> Expr -> Expr
  PNoop :: Expr
  deriving (Typeable)

type Id = String

data Op = Add | Sub | Mul | Eql | NotEql | And | Or | Pipe | Assign | Concat
  deriving (Show, Data)

instance Show Expr where
  show = showWithoutTypes

showWithTypes :: Expr -> String
showWithTypes (PString contents) = "(PString \"" ++ contents ++ "\")"
showWithTypes (Atom ts name) = "(Atom " ++ showTypeSig ts ++ " \"" ++ name ++ "\")"
showWithTypes (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showWithTypes (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showWithTypes (NamedTypeSig ts) = "(NamedTypeSig " ++ showTypeSig ts ++ ")"
showWithTypes (PBool True) = "(PBool True)"
showWithTypes (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ showWithTypes a ++ " " ++ showWithTypes b ++ ")"
showWithTypes PNoop = "(PNoop)"
showWithTypes (PBool False) = "(PBool False)"
showWithTypes (PDict ts pairs) = "(PDict " ++ showTypeSig ts ++ "\") [" ++ showDictContents pairs ++ "])"
showWithTypes (PTuple ts contents) = "(PTuple " ++ showTypeSig ts ++ " [" ++ joinCommaSep contents ++ "])"
showWithTypes (PList ts contents) = "(PList " ++ showTypeSig ts ++ " [" ++ joinCommaSep contents ++ "])"
showWithTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithTypes dict ++ " " ++ showWithTypes update ++ ")"
showWithTypes (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ showWithTypes argList ++ ")"
showWithTypes (PNothing) = "(PNothing)"
showWithTypes (PJust ts e) = "(PJust " ++ showTypeSig ts ++ " " ++ showWithTypes e ++ ")"
showWithTypes (PIf cond e1 e2) = "(PIf " ++ showWithTypes cond ++ " " ++ showWithTypes e1 ++ " " ++ showWithTypes e2 ++ ")"
showWithTypes (App e1 e2) = "(App " ++ showWithTypes e1 ++ " " ++ show e2 ++ ")"
showWithTypes (Lambda ts ids e) = "(Lambda " ++ showTypeSig ts ++ " [" ++ joinCommaSep ids ++ "] " ++ show e ++ ")"
showWithTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithTypes s ++ " " ++ showWithTypes d ++ ")"
showWithTypes _ = "UNKNOWN"

showWithoutTypes :: Expr -> String
showWithoutTypes (PString contents) = "(PString \"" ++ contents ++ "\")"
showWithoutTypes (Atom _ts name) = "(Atom anyTypeSig \"" ++ name ++ "\")"
showWithoutTypes (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showWithoutTypes (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showWithoutTypes (NamedTypeSig ts) = "(NamedTypeSig " ++ showTypeSig ts ++ ")"
showWithoutTypes (PBool True) = "(PBool True)"
showWithoutTypes (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ showWithoutTypes a ++ " " ++ showWithoutTypes b ++ ")"
showWithoutTypes PNoop = "(PNoop)"
showWithoutTypes (PBool False) = "(PBool False)"
showWithoutTypes (PDict _ts pairs) = "(PDict anyTypeSig [" ++ showDictContents pairs ++ "])"
showWithoutTypes (PTuple _ts contents) = "(PTuple anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PList _ts contents) = "(PList anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithoutTypes dict ++ " " ++ showWithoutTypes update ++ ")"
showWithoutTypes (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ showWithoutTypes argList ++ ")"
showWithoutTypes (PNothing) = "(PNothing)"
showWithoutTypes (PJust _ts e) = "(PJust anyTypeSig " ++ showWithoutTypes e ++ ")"
showWithoutTypes (PIf cond e1 e2) = "(PIf " ++ showWithoutTypes cond ++ " " ++ showWithoutTypes e1 ++ " " ++ showWithoutTypes e2 ++ ")"
showWithoutTypes (App e1 e2) = "(App " ++ show e1 ++ " " ++ show e2 ++ ")"
showWithoutTypes (Lambda _ ids e) = "(Lambda anyTypeSig [" ++ joinCommaSep ids ++ "] " ++ show e ++ ")"
showWithoutTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithoutTypes s ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes _ = "UNKNOWN"

-- Val

data Val where
  Function :: (Show e, Evaluatable e) => TypeSig -> Env -> [Expr] -> e -> Val
  Pattern :: [Val] -> Val
  Boolean :: Bool -> Val
  StringVal :: String -> Val
  IntVal :: Integer -> Val
  Dictionary :: Map.Map Val Val -> Val
  FloatVal :: Float -> Val
  DictKey :: String -> Val
  Undefined :: Val
  LJust :: Val -> Val
  LNothing :: Val
  Tuple :: [Val] -> Val
  List :: [Val] -> Val
  deriving (Typeable)

instance Show Val where
  show (Function ts _ ids _) = "<fun TS: " ++ show ts ++ ". Args " ++ intercalate ", " (map showWithTypes ids)
  show Pattern {} = "<pattern>"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show Undefined = "Undefined"
  show (Tuple ns) = "{" ++ List.intercalate ", " (map show ns) ++ "}"
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
  (Tuple a) == (Tuple b) = a == b
  LNothing == LNothing = True
  LJust v1 == LJust v2 = v1 == v2
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (Boolean True) == (Boolean True) = True
  (Boolean False) == (Boolean False) = True
  (List xs) == (List ys) = xs == ys
  (Dictionary m1) == (Dictionary m2) = m1 == m2
  (DictKey a) == (DictKey b) = a == b
  (Function ts1 env1 ids1 e1) == (Function ts2 env2 ids2 e2) =
    ts1 == ts2 && envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful
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

-- TypeSig

data TypeSig = TypeSig {typeSigName :: Maybe String, typeSigIn :: [LangType], typeSigReturn :: LangType} deriving (Show, Eq)

data LangType
  = TakesAnyArgsType
  | ListType LangType
  | IntType
  | FloatType
  | StringType
  | BooleanType
  | DictionaryType
  | DictKeyType
  | MaybeType
  | FunctionType
  | UndefinedType
  | AnyType
  deriving (Show, Eq)

class LangTypeable a where
  toLangType :: a -> LangType

instance LangTypeable String where
  toLangType s = case s of
    "String" -> StringType
    "Integer" -> IntType

instance LangTypeable Val where
  toLangType val = case val of
    IntVal {} -> IntType
    FloatVal {} -> FloatType
    StringVal {} -> StringType
    Function {} -> FunctionType
    Boolean {} -> BooleanType
    Dictionary {} -> DictionaryType
    DictKey {} -> DictKeyType
    Undefined {} -> UndefinedType
    LJust {} -> MaybeType
    LNothing {} -> MaybeType
    Tuple {} -> ListType AnyType
    List {} -> ListType AnyType

showTypeSig TypeSig {typeSigName = name, typeSigIn = inn, typeSigReturn = rtrn} = "(TypeSig {typeSigName = " ++ show name ++ ", typeSigIn = " ++ show inn ++ ", typeSigReturn = " ++ show rtrn ++ "})"

showDictContents pairs = intercalate ", " (map show pairs)

joinCommaSep contents = intercalate ", " (map show contents)
