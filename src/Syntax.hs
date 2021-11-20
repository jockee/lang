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
      withModules :: [String],
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
  Module :: String -> [Expr] -> Expr
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
  Lambda :: (Show e, Evaluatable e) => TypeSig -> ArgsList -> e -> Expr
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
showWithTypes (Module name contents) = "(Module " ++ show name ++ " " ++ intercalate ", " (map showWithTypes contents) ++ ")"
showWithTypes (Atom ts name) = "(Atom " ++ showTypeSig ts ++ " \"" ++ name ++ "\")"
showWithTypes (PString contents) = "(PString \"" ++ contents ++ "\")"
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
showWithTypes (PDictKey key) = "(PDictKey" ++ show key ++ ")"
showWithTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithTypes dict ++ " " ++ showWithTypes update ++ ")"
showWithTypes (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ showWithTypes argList ++ ")"
showWithTypes (PNothing) = "(PNothing)"
showWithTypes (PJust ts e) = "(PJust " ++ showTypeSig ts ++ " " ++ showWithTypes e ++ ")"
showWithTypes (PIf cond e1 e2) = "(PIf " ++ showWithTypes cond ++ " " ++ showWithTypes e1 ++ " " ++ showWithTypes e2 ++ ")"
showWithTypes (App e1 e2) = "(App " ++ showWithTypes e1 ++ " " ++ show e2 ++ ")"
showWithTypes (Lambda ts remainingArgs e) = "(Lambda " ++ showTypeSig ts ++ " [" ++ joinCommaSep remainingArgs ++ "] " ++ show e ++ ")"
showWithTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithTypes s ++ " " ++ showWithTypes d ++ ")"
showWithTypes _ = "UNKNOWN"

showWithoutTypes :: Expr -> String
showWithoutTypes (Module name contents) = "(Module " ++ show name ++ " " ++ intercalate ", " (map showWithoutTypes contents) ++ ")"
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
showWithoutTypes (PDictKey key) = "(PDictKey " ++ show key ++ ")"
showWithoutTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithoutTypes dict ++ " " ++ showWithoutTypes update ++ ")"
showWithoutTypes (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ showWithoutTypes argList ++ ")"
showWithoutTypes (PNothing) = "(PNothing)"
showWithoutTypes (PJust _ts e) = "(PJust anyTypeSig " ++ showWithoutTypes e ++ ")"
showWithoutTypes (PIf cond e1 e2) = "(PIf " ++ showWithoutTypes cond ++ " " ++ showWithoutTypes e1 ++ " " ++ showWithoutTypes e2 ++ ")"
showWithoutTypes (App e1 e2) = "(App " ++ show e1 ++ " " ++ show e2 ++ ")"
showWithoutTypes (Lambda _ remainingArgs e) = "(Lambda anyTypeSig ([" ++ joinCommaSep remainingArgs ++ "]) " ++ show e ++ ")"
showWithoutTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithoutTypes s ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes s = "UNKNOWN"

-- Val

data Val where
  ModuleVal :: String -> Val
  FunctionVal :: (Show e, Evaluatable e) => TypeSig -> Env -> ArgsList -> e -> Val
  Pattern :: [Val] -> Val
  BoolVal :: Bool -> Val
  StringVal :: String -> Val
  IntVal :: Integer -> Val
  DictVal :: Map.Map Val Val -> Val
  FloatVal :: Float -> Val
  DictKey :: String -> Val
  Undefined :: Val
  JustVal :: Val -> Val
  NothingVal :: Val
  TupleVal :: [Val] -> Val
  ListVal :: [Val] -> Val
  deriving (Typeable)

type ArgsList = [Expr]

instance Show Val where
  show (ModuleVal name) = "<module " ++ show name ++ ">"
  show (FunctionVal ts env remainingArgs _) = "<fun \nTS: " ++ show ts ++ "\nArgs: " ++ intercalate ", " (map showWithTypes remainingArgs)
  show (Pattern definitions) = "<pattern " ++ intercalate ", " (map show definitions) ++ ">"
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (TupleVal ns) = "{" ++ List.intercalate ", " (map show ns) ++ "}"
  show (ListVal ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (DictVal m) = "{" ++ List.intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (JustVal v) = "Just " ++ show v
  show (NothingVal) = "Nothing"
  show (DictKey n) = n
  show (StringVal n) = show n
  show (BoolVal n) = show n
  show Undefined = "Undefined"

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
  (TupleVal a) == (TupleVal b) = a == b
  NothingVal == NothingVal = True
  JustVal v1 == JustVal v2 = v1 == v2
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (BoolVal True) == (BoolVal True) = True
  (BoolVal False) == (BoolVal False) = True
  (ListVal xs) == (ListVal ys) = xs == ys
  (DictVal m1) == (DictVal m2) = m1 == m2
  (DictKey a) == (DictKey b) = a == b
  (FunctionVal ts1 env1 ids1 e1) == (FunctionVal ts2 env2 ids2 e2) =
    ts1 == ts2 && envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful
  _ == _ = False

instance Arith Val where
  cmpOp ">" = \a b -> BoolVal $ a > b
  cmpOp "<" = \a b -> BoolVal $ a < b
  cmpOp ">=" = \a b -> BoolVal $ a >= b
  cmpOp "<=" = \a b -> BoolVal $ a <= b
  evalOp Add = (+)
  evalOp Sub = (-)
  evalOp Mul = (*)
  evalOp Eql = \a b -> BoolVal $ a == b
  evalOp NotEql = \a b -> BoolVal $ a /= b
  evalOp And = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a && b
    _ -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a || b
    _ -> BoolVal False

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
  | AtomType
  | AnyType
  deriving (Show, Eq)

class LangTypeable a where
  toLangType :: a -> LangType

instance LangTypeable String where
  toLangType s = case s of
    "String" -> StringType
    "Integer" -> IntType

instance LangTypeable Expr where
  toLangType e = case e of
    PTuple {} -> ListType AnyType
    PList {} -> ListType AnyType
    PDict {} -> DictionaryType
    PInteger {} -> IntType
    PFloat {} -> FloatType
    PJust {} -> MaybeType
    PNothing {} -> MaybeType
    PString {} -> StringType
    PBool {} -> BooleanType
    PRange {} -> ListType AnyType
    Atom {} -> AtomType
    PDictUpdate {} -> UndefinedType
    DictAccess {} -> UndefinedType
    PDictKey {} -> UndefinedType
    PIf {} -> UndefinedType
    InternalFunction {} -> UndefinedType
    Lambda {} -> UndefinedType
    App {} -> UndefinedType
    Binop {} -> UndefinedType
    Cmp {} -> UndefinedType
    NamedTypeSig {} -> UndefinedType
    PNoop {} -> UndefinedType

instance LangTypeable Val where
  toLangType val = case val of
    IntVal {} -> IntType
    FloatVal {} -> FloatType
    StringVal {} -> StringType
    FunctionVal {} -> FunctionType
    BoolVal {} -> BooleanType
    DictVal {} -> DictionaryType
    DictKey {} -> DictKeyType
    Undefined {} -> UndefinedType
    JustVal {} -> MaybeType
    NothingVal {} -> MaybeType
    TupleVal {} -> ListType AnyType
    ListVal {} -> ListType AnyType

showTypeSig TypeSig {typeSigName = name, typeSigIn = inn, typeSigReturn = rtrn} = "(TypeSig {typeSigName = " ++ show name ++ ", typeSigIn = " ++ show inn ++ ", typeSigReturn = " ++ show rtrn ++ "})"

showDictContents pairs = intercalate ", " (map show pairs)

joinCommaSep contents = intercalate ", " (map show contents)
