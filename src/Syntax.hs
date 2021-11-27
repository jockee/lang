{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Syntax where

import Data.Bifunctor
import Data.Data
import Data.List as List
import Data.Map qualified as Map
import Data.Typeable
import GHC.Real

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

defaultEnvScopes :: [String]
defaultEnvScopes = ["global"]

emptyEnv :: Env
emptyEnv = Env {envValues = Map.empty, envScopes = defaultEnvScopes, typeSigs = Map.empty}

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
  PDataDefinition :: DataConstructor -> [ConstructorWithArgs] -> Expr
  PDataConstructor :: Name -> [Expr] -> Expr
  PTrait :: Name -> [Expr] -> Expr
  PImplementation :: Name -> DataConstructor -> [Expr] -> Expr
  PDictUpdate :: Expr -> Expr -> Expr
  DictAccess :: Expr -> Expr -> Expr
  PCase :: TypeSig -> Expr -> [Case] -> Expr
  PDictKey :: String -> Expr
  ConsList :: [String] -> Expr
  PInteger :: Integer -> Expr
  PFloat :: Double -> Expr
  PString :: [Expr] -> Expr
  PChar :: String -> Expr
  PBool :: Bool -> Expr
  InternalFunction :: Id -> Expr -> Expr
  Lambda :: (Show e, Evaluatable e) => TypeSig -> ArgsList -> e -> Expr
  App :: (Show e, Evaluatable e) => Expr -> e -> Expr
  Binop :: Op -> Expr -> Expr -> Expr
  Unaryop :: UnOp -> Expr -> Expr
  Cmp :: String -> Expr -> Expr -> Expr
  PTypeSig :: TypeSig -> Expr
  PRange :: TypeSig -> Expr -> Expr -> Expr
  PNoop :: Expr
  deriving (Typeable)

type Id = String

type Case = (Expr, Expr)

data Op = Add | Sub | Mul | Div | Eql | NotEql | Mod | And | Or | Pipe | Assign | Concat | Cons | Pow
  deriving (Show, Data)

data UnOp = ToFloat | ToInteger | Sqrt | Not | Floor | Round | Ceiling | Abs
  deriving (Show, Data)

instance Show Expr where
  show = showWithoutTypes

showWithTypes :: Expr -> String
showWithTypes (Module name contents) = "(Module " ++ show name ++ " " ++ intercalate ", " (map showWithTypes contents) ++ ")"
showWithTypes (Atom ts name) = "(Atom " ++ showTypeSig ts ++ " \"" ++ name ++ "\")"
showWithTypes (PTrait name defs) = "(PTrait " ++ show name ++ " [" ++ intercalate ", " (map show defs) ++ "])"
showWithTypes (PImplementation trait dtype defs) = "(PImplementation " ++ show trait ++ " " ++ show dtype ++ " [" ++ intercalate ", " (map show defs) ++ "])"
showWithTypes (PDataConstructor name args) = "(PDataConstructor " ++ show name ++ " [" ++ intercalate ", " (map show args) ++ "])"
showWithTypes (PDataDefinition name args) = "(PDataDefinition " ++ show name ++ " [" ++ intercalate ", " (map show args) ++ "])"
showWithTypes (PString parts) = "(PString [" ++ intercalate "," (map show parts) ++ "])"
showWithTypes (PChar contents) = "(PChar \"" ++ contents ++ "\")"
showWithTypes (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showWithTypes (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showWithTypes (PTypeSig ts) = "(PTypeSig " ++ showTypeSig ts ++ ")"
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
showWithTypes (ConsList cs) = "(ConsList [" ++ intercalate "," (map show cs) ++ "])"
showWithTypes (PCase ts cond cases) = "(PCase " ++ showWithTypes cond ++ " " ++ show cases ++ ")"
showWithTypes (App e1 e2) = "(App " ++ showWithTypes e1 ++ " " ++ show e2 ++ ")"
showWithTypes (Lambda ts remainingArgs e) = "(Lambda " ++ showTypeSig ts ++ " [" ++ joinCommaSep remainingArgs ++ "] " ++ show e ++ ")"
showWithTypes (Unaryop t d) = "(Unaryop " ++ show t ++ " " ++ showWithTypes d ++ ")"
showWithTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithTypes s ++ " " ++ showWithTypes d ++ ")"
showWithTypes _ = "UNKNOWN"

showWithoutTypes :: Expr -> String
showWithoutTypes (Atom _ts name) = "(Atom anyTypeSig \"" ++ name ++ "\")"
showWithoutTypes (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ showWithoutTypes a ++ " " ++ showWithoutTypes b ++ ")"
showWithoutTypes (PDict _ts pairs) = "(PDict anyTypeSig [" ++ showDictContents pairs ++ "])"
showWithoutTypes (PTuple _ts contents) = "(PTuple anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PList _ts contents) = "(PList anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithoutTypes dict ++ " " ++ showWithoutTypes update ++ ")"
showWithoutTypes (InternalFunction f argList) = "(InternalFunction " ++ show f ++ " " ++ showWithoutTypes argList ++ ")"
showWithoutTypes (App e1 e2) = "(App " ++ show e1 ++ " " ++ show e2 ++ ")"
showWithoutTypes (Lambda _ remainingArgs e) = "(Lambda anyTypeSig ([" ++ joinCommaSep remainingArgs ++ "]) " ++ show e ++ ")"
showWithoutTypes (Unaryop t d) = "(Unaryop " ++ show t ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithoutTypes s ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes s = showWithTypes s

-- Val

data Val where
  ModuleVal :: String -> Val
  FunctionVal :: (Show e, Evaluatable e) => TypeSig -> Env -> ArgsList -> e -> Val
  DataVal :: DataConstructor -> Name -> [Val] -> Val
  DataConstructorDefinitionVal :: DataConstructor -> [String] -> Val
  Pattern :: [Val] -> Val
  BoolVal :: Bool -> Val
  StringVal :: String -> Val
  IntVal :: Integer -> Val
  DictVal :: Map.Map Val Val -> Val
  FloatVal :: Double -> Val
  DictKey :: String -> Val
  Undefined :: Val
  TupleVal :: [Val] -> Val
  ListVal :: [Val] -> Val
  TraitVal :: Name -> [Expr] -> Val -- change from Expr? Just sliming tests
  deriving (Typeable)

type ArgsList = [Expr]

type DataConstructor = String

type Trait = String

type TypeConstructor = String

type Name = String

type ConstructorWithArgs = (String, [String])

instance Show Val where
  show (ModuleVal name) = "<module " ++ show name ++ ">"
  show (FunctionVal ts env remainingArgs _) = "<fun>"
  -- show (FunctionVal ts env remainingArgs _) = "<fun \nTS: " ++ show ts ++ "\nArgs: " ++ intercalate ", " (map showWithTypes remainingArgs)
  show (Pattern definitions) = "<pattern " ++ intercalate ", " (map show definitions) ++ ">"
  show (DataConstructorDefinitionVal n args) = "DataConstructorDefinitionVal " ++ show n ++ " " ++ show args
  show (DataVal dtype n args) = n ++ " " ++ joinCommaSep args
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (TupleVal ns) = "(" ++ List.intercalate ", " (map show ns) ++ ")"
  show (ListVal ns) = "[" ++ List.intercalate ", " (map show ns) ++ "]"
  show (DictVal m) = "{" ++ List.intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (DictKey n) = n
  show (TraitVal name defs) = "TraitVal " ++ show name ++ " " ++ List.intercalate ", " (map show defs)
  show (StringVal n) = show n
  show (BoolVal n)
    | n = "true"
    | otherwise = "false"
  show Undefined = "Undefined"

class Num a => Arith a where
  cmpOp :: String -> (a -> a -> Val)
  evalOp :: Op -> (a -> a -> Val)
  evalUnOp :: UnOp -> (a -> Val)

instance Num Val where
  (FloatVal i1) + (FloatVal i2) = FloatVal (i1 + i2)
  (IntVal i1) + (IntVal i2) = IntVal (i1 + i2)
  (FloatVal i1) * (FloatVal i2) = FloatVal (i1 * i2)
  (IntVal i1) * (IntVal i2) = IntVal (i1 * i2)
  (FloatVal i1) - (FloatVal i2) = FloatVal (i1 - i2)
  (IntVal i1) - (IntVal i2) = IntVal (i1 - i2)
  signum x = case x of
    (FloatVal i1) -> if i1 >= 0 then 1 else -1
    (IntVal i1) -> if i1 >= 0 then 1 else -1
  abs (FloatVal i1) = FloatVal $ abs i1
  abs (IntVal i1) = IntVal $ abs i1
  fromInteger i = IntVal i

instance Ord Val where
  compare (DictKey i1) (DictKey i2) = compare i1 i2
  compare (FloatVal i1) (FloatVal i2) = compare i1 i2
  compare (FloatVal i1) (IntVal i2) = compare i1 (fromIntegral i2)
  compare (IntVal i1) (FloatVal i2) = compare (fromIntegral i1) i2
  compare (IntVal i1) (IntVal i2) = compare i1 i2
  compare a b = error ("not implemented" ++ show a ++ " " ++ show b)

instance Eq Val where
  (TupleVal a) == (TupleVal b) = a == b
  StringVal s1 == StringVal s2 = s1 == s2
  (FloatVal i1) == (FloatVal i2) = i1 == i2
  (IntVal i1) == (IntVal i2) = i1 == i2
  (DataVal dtype1 n1 args1) == (DataVal dtype2 n2 args2) = dtype1 == dtype2 && n1 == n2 && args1 == args2
  (BoolVal True) == (BoolVal True) = True
  (BoolVal False) == (BoolVal False) = True
  (ListVal xs) == (ListVal ys) = xs == ys
  (DictVal m1) == (DictVal m2) = m1 == m2
  (DictKey a) == (DictKey b) = a == b
  (FunctionVal ts1 env1 ids1 e1) == (FunctionVal ts2 env2 ids2 e2) =
    ts1 == ts2 && envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful envValues env1 == envValues env2 -- for testing purposes. lambda function equality is probably not very useful
  _ == _ = False

instance Real Val where
  toRational (IntVal x) = x :% 1

instance RealFrac Val where
  properFraction (FloatVal x) = FloatVal <$> properFraction x
  properFraction (IntVal x) = FloatVal <$> properFraction (fromIntegral x)

instance Enum Val where
  fromEnum (FloatVal x) = fromIntegral $ round x :: Int
  toEnum x = FloatVal (fromIntegral x :: Double)

instance Integral Val where
  toInteger (IntVal i1) = i1
  toInteger (FloatVal i1) = round i1
  quotRem (IntVal i1) (IntVal i2) = bimap IntVal IntVal $ quotRem i1 i2

instance Fractional Val where
  (/) (FloatVal i1) (FloatVal i2) = FloatVal $ i1 / i2
  (/) (IntVal i1) (IntVal i2) = FloatVal $ fromIntegral i1 / fromIntegral i2
  (/) (IntVal i1) (FloatVal i2) = FloatVal $ fromIntegral i1 / i2
  (/) (FloatVal i1) (IntVal i2) = FloatVal $ i1 / fromIntegral i2
  fromRational x = FloatVal $ fromRational x

instance Arith Val where
  cmpOp ">" = \a b -> BoolVal $ a > b
  cmpOp "<" = \a b -> BoolVal $ a < b
  cmpOp ">=" = \a b -> BoolVal $ a >= b
  cmpOp "<=" = \a b -> BoolVal $ a <= b
  evalUnOp ToFloat = \x -> FloatVal (fromIntegral x :: Double)
  evalUnOp ToInteger = IntVal . round
  evalUnOp Sqrt = FloatVal . sqrt . fromIntegral
  evalUnOp Round = IntVal . round
  evalUnOp Floor = IntVal . floor
  evalUnOp Ceiling = IntVal . ceiling
  evalUnOp Abs = \case
    IntVal i -> IntVal $ abs i
    FloatVal f -> FloatVal $ abs f
  evalOp Add = (+)
  evalOp Pow = (^^)
  evalOp Sub = (-)
  evalOp Mod = mod
  evalOp Div = (/)
  evalOp Mul = (*)
  evalOp Eql = \a b -> BoolVal $ a == b
  evalOp NotEql = \a b -> BoolVal $ a /= b
  evalOp And = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a && b
    _ -> BoolVal False
  evalOp Or = \a b -> case (a, b) of
    (BoolVal a, BoolVal b) -> BoolVal $ a || b
    _ -> BoolVal False
  evalOp s = error $ show s

-- TypeSig

data TypeSig = TypeSig
  { typeSigName :: Maybe String,
    typeSigTraitBinding :: Maybe String,
    typeSigIn :: [LangType],
    typeSigReturn :: LangType
  }
  deriving (Show, Eq)

data LangType
  = ListType LangType
  | IntType
  | FloatType
  | StringType
  | BooleanType
  | DictionaryType
  | DictKeyType
  | FunctionType [LangType] LangType
  | DataConstructorType String
  | TypeConstructorType Name LangType
  | UndefinedType
  | AtomType
  | AnyType
  deriving (Show, Eq, Data, Typeable)

prettyLangType (TypeConstructorType name _) = name
prettyLangType (DataConstructorType dtype) = dtype
prettyLangType x = show $ toConstr x

class LangTypeable a where
  toLangType :: a -> LangType

instance LangTypeable String where
  toLangType s = case s of
    "String" -> StringType
    "Integer" -> IntType
    "Float" -> FloatType
    "Boolean" -> BooleanType
    _ -> AnyType

instance LangTypeable Expr where
  toLangType e = case e of
    PTuple {} -> ListType AnyType
    PList {} -> ListType AnyType
    PDict {} -> DictionaryType
    PInteger {} -> IntType
    PFloat {} -> FloatType
    PString {} -> StringType
    PBool {} -> BooleanType
    PRange {} -> ListType AnyType
    Atom {} -> AtomType
    PDictUpdate {} -> UndefinedType
    DictAccess {} -> UndefinedType
    PDictKey {} -> UndefinedType
    PCase {} -> UndefinedType
    InternalFunction {} -> UndefinedType
    Lambda {} -> UndefinedType
    App {} -> UndefinedType
    Unaryop {} -> UndefinedType
    Binop {} -> UndefinedType
    Cmp {} -> UndefinedType
    PTypeSig {} -> UndefinedType
    PNoop {} -> UndefinedType
    ConsList {} -> UndefinedType
    PDataConstructor name _ -> DataConstructorType name
    PTrait {} -> UndefinedType
    s -> error (show s)

instance LangTypeable Val where
  toLangType val = case val of
    IntVal {} -> IntType
    FloatVal {} -> FloatType
    StringVal {} -> StringType
    FunctionVal {} -> FunctionType [UndefinedType] UndefinedType
    BoolVal {} -> BooleanType
    DictVal {} -> DictionaryType
    DictKey {} -> DictKeyType
    Undefined {} -> UndefinedType
    TupleVal {} -> ListType AnyType
    ListVal {} -> ListType AnyType
    DataVal cons name _ -> TypeConstructorType cons (toLangType name)
    s -> error $ "Not implemented" ++ show s

showTypeSig TypeSig {typeSigName = name, typeSigIn = inn, typeSigReturn = rtrn} = "(TypeSig {typeSigName = " ++ show name ++ ", typeSigIn = " ++ show inn ++ ", typeSigReturn = " ++ show rtrn ++ "})"

showDictContents pairs = intercalate ", " (map show pairs)

joinCommaSep contents = intercalate ", " (map show contents)
