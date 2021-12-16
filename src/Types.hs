{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types where

import Data.Aeson
import Data.Aeson.Types qualified as AT
import Data.Bifunctor
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Data
import Data.HashMap.Internal.Strict qualified as HM
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Scientific qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Real

-- ENV

data Env where
  Env ::
    { inModule :: Maybe Module,
      envLambdaEnvs :: [LambdaEnv],
      includedModules :: [Module],
      stdLibBindings :: VarMap,
      envInStdLib :: Bool,
      envBindings :: VarMap,
      typeSigs :: Map.Map String TypeSig,
      envLangPath :: String
    } ->
    Env
  deriving stock (Show)

data LambdaEnv where
  LambdaEnv ::
    { lambdaEnvBindings :: VarMap
    } ->
    LambdaEnv
  deriving stock (Show)

class Show e => HasBindings e where
  setBindings :: e -> VarMap -> e
  getBindings :: e -> VarMap
  bindingsType :: e -> String

instance HasBindings Env where
  setBindings env v =
    if envInStdLib env
      then env {stdLibBindings = v}
      else env {envBindings = v}
  getBindings env =
    if envInStdLib env
      then stdLibBindings env
      else envBindings env
  bindingsType _ = "Env"

instance HasBindings LambdaEnv where
  setBindings lambdaEnv v = lambdaEnv {lambdaEnvBindings = v}
  getBindings lambdaEnv = lambdaEnvBindings lambdaEnv
  bindingsType _ = "LambdaEnv"

type VarMap = Map.Map String [EnvEntry]

type Module = String

data EnvEntry = EnvEntry
  { envEntryModule :: Maybe Module,
    envEntryValue :: Val
  }
  deriving stock (Show, Eq)

instance Eq Env where
  Env {typeSigs = t1, envBindings = s1} == Env {typeSigs = t2, envBindings = s2} = t1 == t2 && s1 == s2

instance Eq LambdaEnv where
  LambdaEnv {lambdaEnvBindings = b1} == LambdaEnv {lambdaEnvBindings = b2} = b1 == b2

emptyEnv :: Env
emptyEnv = Env {envBindings = Map.empty, envLambdaEnvs = [], inModule = Nothing, typeSigs = Map.empty, envLangPath = "", stdLibBindings = Map.empty, includedModules = [], envInStdLib = False}

emptyLambdaEnv :: LambdaEnv
emptyLambdaEnv = LambdaEnv {lambdaEnvBindings = Map.empty}

-- Evaluatable

class Show e => Evaluatable e where
  toVal :: e -> Val
  toExpr :: e -> Expr

instance Evaluatable Val where
  toVal val = val
  toExpr val = Evaluated val

instance Evaluatable Expr where
  toExpr expr = expr
  toVal _expr = undefined

-- Expr

data Expr where
  Module :: String -> [Expr] -> Expr
  Atom :: TypeSig -> String -> Expr
  PTuple :: TypeSig -> [Expr] -> Expr
  PList :: TypeSig -> [Expr] -> Expr
  PDict :: TypeSig -> [(Expr, Expr)] -> Expr
  PDictKey :: String -> Expr
  PDictUpdate :: Expr -> Expr -> Expr
  PDictKeyLookup :: String -> Expr
  DictAccess :: Expr -> Expr -> Expr
  PDataDefinition :: DataConstructor -> [ConstructorWithArgs] -> Expr
  PDataConstructor :: Name -> [Expr] -> Expr
  PTrait :: Name -> [Expr] -> [Expr] -> Expr
  PImplementation :: Name -> DataConstructor -> [Expr] -> Expr
  PCase :: TypeSig -> Expr -> [Case] -> Expr
  Cons :: [Expr] -> Expr
  PInteger :: Integer -> Expr
  PFloat :: Double -> Expr
  PImport :: Expr -> Expr
  PInterpolatedString :: [Expr] -> Expr
  PString :: T.Text -> Expr
  PBool :: Bool -> Expr
  HFI :: Id -> Expr -> Expr
  Lambda :: (Show e, Evaluatable e) => LambdaEnv -> TypeSig -> ArgsList -> e -> Expr
  App :: (Show e, Evaluatable e) => Expr -> e -> Expr
  Binop :: Op -> Expr -> Expr -> Expr
  Unaryop :: UnOp -> Expr -> Expr
  Cmp :: String -> Expr -> Expr -> Expr
  PTypeSig :: TypeSig -> Expr
  PRange :: TypeSig -> Expr -> Expr -> Expr
  PNoop :: Expr
  Block :: [Expr] -> Expr
  Evaluated :: Val -> Expr -- NOTE: Only needed to "uneval" a pattern
  PatternExpr :: [Val] -> Expr -- NOTE: Only needed to "uneval" a pattern

instance Eq Expr where
  PNoop == PNoop = True
  _ == _ = False -- NOTE: this may cause issues

type Id = String

type Case = (Expr, Expr)

data Op = AddOrConcat | Add | Sub | Mul | Div | Eql | NotEql | Mod | And | Or | Pipe | MapPipe | Assign | Pow
  deriving stock (Show, Data)

data UnOp = ToFloat | ToInteger | Sqrt | Not | Floor | Round | Ceiling | Abs
  deriving stock (Show, Data)

instance Show Expr where
  show = showWithoutTypes

showWithTypes :: Expr -> String
showWithTypes (Module name contents) = "(Module " ++ show name ++ " " ++ joinCommaSep contents ++ ")"
showWithTypes (Atom ts name) = "(Atom " ++ showTypeSig ts ++ " \"" ++ name ++ "\")"
showWithTypes (PTrait name types funs) = "(PTrait " ++ show name ++ " [" ++ joinCommaSep types ++ "] [" ++ joinCommaSep funs ++ "])"
showWithTypes (PImplementation trait dtype defs) = "(PImplementation " ++ show trait ++ " " ++ show dtype ++ " [" ++ joinCommaSep defs ++ "])"
showWithTypes (PDataConstructor name args) = "(PDataConstructor " ++ show name ++ " [" ++ joinCommaSep args ++ "])"
showWithTypes (PDataDefinition name args) = "(PDataDefinition " ++ show name ++ " [" ++ joinCommaSep args ++ "])"
showWithTypes (PInterpolatedString parts) = "(PInterpolatedString [" ++ joinCommaSep parts ++ "])"
showWithTypes (PString contents) = "(PString \"" ++ T.unpack contents ++ "\")"
showWithTypes (PInteger contents) = "(PInteger " ++ show contents ++ ")"
showWithTypes (PFloat contents) = "(PFloat " ++ show contents ++ ")"
showWithTypes (PTypeSig ts) = "(PTypeSig " ++ showTypeSig ts ++ ")"
showWithTypes (PBool True) = "(PBool True)"
showWithTypes (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ showWithTypes a ++ " " ++ showWithTypes b ++ ")"
showWithTypes PNoop = "(PNoop)"
showWithTypes (PBool False) = "(PBool False)"
showWithTypes (PDict ts pairs) = "(PDict " ++ showTypeSig ts ++ "\") [" ++ joinCommaSep pairs ++ "])"
showWithTypes (PTuple ts contents) = "(PTuple " ++ showTypeSig ts ++ " [" ++ joinCommaSep contents ++ "])"
showWithTypes (PList ts contents) = "(PList " ++ showTypeSig ts ++ " [" ++ joinCommaSep contents ++ "])"
showWithTypes (DictAccess key dict) = "(DictAccess " ++ show key ++ " " ++ show dict ++ ")"
showWithTypes (PDictKey key) = "(PDictKey " ++ show key ++ ")"
showWithTypes (PDictKeyLookup key) = "(PDictKeyLookup " ++ show key ++ ")"
showWithTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithTypes dict ++ " " ++ showWithTypes update ++ ")"
showWithTypes (HFI f argList) = "(HFI " ++ show f ++ " " ++ showWithTypes argList ++ ")"
showWithTypes (Cons cs) = "(Cons [" ++ joinCommaSep cs ++ "])"
showWithTypes (PCase _ts cond cases) = "(PCase " ++ showWithTypes cond ++ " " ++ show cases ++ ")"
showWithTypes (App e1 e2) = "(App " ++ showWithTypes e1 ++ " " ++ show e2 ++ ")"
showWithTypes (Lambda lambdaEnv ts remainingArgs e) = "(Lambda " ++ show lambdaEnv ++ " " ++ showTypeSig ts ++ " [" ++ joinCommaSep remainingArgs ++ "] " ++ show e ++ ")"
showWithTypes (Unaryop t d) = "(Unaryop " ++ show t ++ " " ++ showWithTypes d ++ ")"
showWithTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithTypes s ++ " " ++ showWithTypes d ++ ")"
showWithTypes (PatternExpr defs) = "PatternExpr"
showWithTypes (Block exprs) = "(Block [" ++ joinCommaSep exprs ++ "])"
showWithTypes (Evaluated defs) = "Evaluated"
showWithTypes _ = "UNKNOWN"

showWithoutTypes :: Expr -> String
showWithoutTypes (Atom _ts name) = "(Atom anyTypeSig \"" ++ name ++ "\")"
showWithoutTypes (Cmp s a b) = "(Cmp " ++ show s ++ " " ++ showWithoutTypes a ++ " " ++ showWithoutTypes b ++ ")"
showWithoutTypes (PDict _ts pairs) = "(PDict anyTypeSig [" ++ joinCommaSep pairs ++ "])"
showWithoutTypes (PTuple _ts contents) = "(PTuple anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PList _ts contents) = "(PList anyTypeSig [" ++ joinCommaSep contents ++ "])"
showWithoutTypes (PDictUpdate dict update) = "(PDictUpdate " ++ showWithoutTypes dict ++ " " ++ showWithoutTypes update ++ ")"
showWithoutTypes (HFI f argList) = "(HFI " ++ show f ++ " " ++ showWithoutTypes argList ++ ")"
showWithoutTypes (App e1 e2) = "(App " ++ show e1 ++ " " ++ show e2 ++ ")"
showWithoutTypes (Lambda _ _ remainingArgs e) = "(Lambda emptyLambdaEnv anyTypeSig ([" ++ joinCommaSep remainingArgs ++ "]) " ++ show e ++ ")"
showWithoutTypes (Unaryop t d) = "(Unaryop " ++ show t ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes (Binop t s d) = "(Binop " ++ show t ++ " " ++ showWithoutTypes s ++ " " ++ showWithoutTypes d ++ ")"
showWithoutTypes s = showWithTypes s

-- Val

data Val where
  ModuleVal :: String -> Val
  FunctionVal :: (Show e, Evaluatable e) => LambdaEnv -> TypeSig -> ArgsList -> e -> Val
  DataVal :: DataConstructor -> Name -> [Val] -> Val
  DataConstructorDefinitionVal :: DataConstructor -> [String] -> Val
  Pattern :: [Val] -> Val
  BoolVal :: Bool -> Val
  StringVal :: T.Text -> Val
  IntVal :: Integer -> Val
  DictVal :: Map.Map Val Val -> Val
  FloatVal :: Double -> Val
  DictKey :: String -> Val
  Undefined :: Val
  TupleVal :: [Val] -> Val
  ListVal :: [Val] -> Val
  TraitVal :: Name -> [Expr] -> Val

jsonToVal :: ByteString -> Val
jsonToVal json = toVal (decode json :: Maybe Value)
  where
    toVal (Just (AT.String s)) = StringVal s
    toVal (Just AT.Null) = DataVal "Maybe" "None" []
    toVal (Just (AT.Object xs)) = DictVal $ Map.fromList $ map (\(k, v) -> (DictKey $ T.unpack k, (toVal . Just) v)) (HM.toList xs)
    toVal (Just (AT.Array xs)) = ListVal $ map (toVal . Just) $ V.toList xs
    toVal (Just (Bool b)) = BoolVal b
    toVal (Just (AT.Number s)) = case S.floatingOrInteger s of
      Left f -> FloatVal f
      Right i -> IntVal i
    toVal Nothing = DataVal "Maybe" "None" []
    toVal s = error $ show s

instance ToJSON Val where
  toJSON (BoolVal b) = AT.Bool b
  toJSON (StringVal s) = AT.String s
  toJSON (IntVal s) = AT.Number $ S.scientific s 0
  toJSON (FloatVal s) = AT.Number $ S.fromFloatDigits s
  toJSON (ListVal xs) = AT.Array $ V.fromList $ map toJSON xs
  toJSON (DataVal "Maybe" "Some" [x]) = toJSON x
  toJSON (DataVal "Maybe" "None" _) = AT.Null
  toJSON (TupleVal xs) = AT.Array $ V.fromList $ map toJSON xs
  toJSON Undefined = AT.Null
  toJSON (DictVal d) = object $ map (\(DictKey s, v) -> (T.pack s, toJSON v)) $ Map.toList d

type ArgsList = [Expr]

type DataConstructor = String

type Trait = String

type TypeConstructor = String

type Name = String

type ConstructorWithArgs = (String, [String])

instance Show Val where
  show (ModuleVal name) = "<module " ++ show name ++ ">"
  show (FunctionVal _lambdaEnv ts remainingArgs _) = "(FunctionVal " ++ show ts ++ " " ++ show remainingArgs ++ ")\n"
  show (Pattern definitions) = "<pattern " ++ joinCommaSep definitions ++ ">"
  show (DataConstructorDefinitionVal n args) = "DataConstructorDefinitionVal " ++ show n ++ " " ++ show args
  show (DataVal dtype n args) = "(DataVal " ++ show dtype ++ " " ++ show n ++ " [" ++ joinCommaSep args ++ "])"
  show (IntVal n) = "(IntVal " ++ show n ++ ")"
  show (FloatVal n) = "(FloatVal " ++ show n ++ ")"
  show (TupleVal ns) = "(TupleVal (" ++ joinCommaSep ns ++ "))"
  show (ListVal ns) = "(ListVal [" ++ joinCommaSep ns ++ "])"
  show (DictVal m) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)) ++ "}"
  show (DictKey n) = "(DictKey " ++ show n ++ ")"
  show (TraitVal name defs) = "(TraitVal " ++ show name ++ " " ++ joinCommaSep defs ++ ")"
  show (StringVal n) = "(StringVal " ++ show n ++ ")"
  show (BoolVal n) = "(BoolVal " ++ show n ++ ")"
  show Undefined = "Undefined"

-- for use in repl, for instance
prettyVal :: Val -> String
prettyVal (ModuleVal name) = "<module " ++ show name ++ ">"
prettyVal (FunctionVal _ts _lambdaEnv _remainingArgs _) = "<fun>"
prettyVal (Pattern definitions) = "<pattern " ++ joinCommaSep definitions ++ ">"
prettyVal (DataConstructorDefinitionVal n args) = "DataConstructorDefinitionVal " ++ show n ++ " " ++ show args
prettyVal (DataVal _dtype n args) = n ++ (if null args then "" else " " ++ joinCommaSepWithShowFun prettyVal args)
prettyVal (IntVal n) = show n
prettyVal (FloatVal n) = show n
prettyVal (TupleVal ns) = "(" ++ joinCommaSep ns ++ ")"
prettyVal (ListVal ns) = "[" ++ joinCommaSep ns ++ "]"
prettyVal (DictVal m) = "{" ++ intercalate "," (map (\(k, v) -> prettyVal k ++ ": " ++ prettyVal v) (Map.toList m)) ++ "}"
prettyVal (DictKey n) = show n
prettyVal (TraitVal name defs) = "TraitVal " ++ show name ++ " " ++ joinCommaSep defs
prettyVal (StringVal n) = show $ T.unpack n
prettyVal (BoolVal n)
  | n = "true"
  | otherwise = "false"
prettyVal Undefined = "Undefined"

-- for use in stdout, for instance
showRaw :: Val -> String
showRaw (StringVal n) = T.unpack n
showRaw s = prettyVal s

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
  compare (StringVal i1) (StringVal i2) = compare i1 i2
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
  (FunctionVal lambdaEnv1 ts1 _ids1 _e1) == (FunctionVal lambdaEnv2 ts2 _ids2 _e2) =
    ts1 == ts2 && lambdaEnvBindings lambdaEnv1 == lambdaEnvBindings lambdaEnv2 -- for testing purposes. lambda function equality is probably not very useful.
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
    typeSigModule :: Maybe String,
    typeSigImplementationBinding :: Maybe String,
    typeSigTraitBinding :: Maybe Trait,
    typeSigIn :: [LangType],
    typeSigReturn :: LangType
  }
  deriving stock (Show, Eq)

anyTypeSig :: TypeSig
anyTypeSig = TypeSig {typeSigName = Nothing, typeSigModule = Nothing, typeSigTraitBinding = Nothing, typeSigImplementationBinding = Nothing, typeSigIn = [], typeSigReturn = AnyType}

data LangType
  = ListType LangType
  | TupleType LangType
  | IntType
  | FloatType
  | StringType
  | BooleanType
  | DictType
  | DictKeyType
  | FunctionType [LangType] LangType
  | TraitVariableType String LangType
  | DataConstructorType String
  | TypeConstructorType String LangType
  | PatternType [LangType]
  | UndefinedType
  | AnyType
  deriving stock (Show, Eq, Data)

prettyLangType :: LangType -> String
prettyLangType (TypeConstructorType name _) = name
prettyLangType (DataConstructorType dtype) = dtype
prettyLangType FloatType = "Float"
prettyLangType (ListType t) = "List<" ++ prettyLangType t ++ ">"
prettyLangType IntType = "Integer"
prettyLangType StringType = "String"
prettyLangType BooleanType = "Boolean"
prettyLangType DictType = "Dict"
prettyLangType DictKeyType = "DictKey"
prettyLangType (FunctionType lts lt) = "(" ++ joinCommaSep lts ++ ": " ++ prettyLangType lt ++ ")"
prettyLangType UndefinedType = "Undefined"
prettyLangType AnyType = "Any"
prettyLangType x = show $ toConstr x

class LangTypeable a where
  toLangType :: a -> LangType

instance LangTypeable String where
  toLangType s = case s of
    "String" -> StringType
    "Integer" -> IntType
    "List" -> ListType AnyType
    "Float" -> FloatType
    "Boolean" -> BooleanType
    "Dict" -> DictType
    _ -> AnyType

instance LangTypeable Expr where
  toLangType e = case e of
    PTuple {} -> TupleType AnyType
    PList {} -> ListType AnyType
    PDict {} -> DictType
    PInteger {} -> IntType
    PFloat {} -> FloatType
    PInterpolatedString {} -> StringType
    PString {} -> StringType
    PBool {} -> BooleanType
    PRange {} -> ListType AnyType
    Atom {} -> AnyType
    PDictUpdate {} -> UndefinedType
    DictAccess {} -> UndefinedType
    PDictKey {} -> UndefinedType
    PCase {} -> UndefinedType
    HFI {} -> UndefinedType
    Lambda {} -> UndefinedType
    App {} -> UndefinedType
    Unaryop {} -> UndefinedType
    Binop {} -> UndefinedType
    Cmp {} -> UndefinedType
    PTypeSig {} -> UndefinedType
    PNoop {} -> UndefinedType
    Cons {} -> ListType AnyType
    PDataConstructor name _ -> DataConstructorType name
    PTrait {} -> UndefinedType
    s -> error (show s)

instance LangTypeable Val where
  toLangType (FunctionVal _ ts _ _) = FunctionType (typeSigIn ts) (typeSigReturn ts)
  toLangType (Pattern defs) = PatternType $ map (\case (FunctionVal _ ts _ _) -> FunctionType (typeSigIn ts) (typeSigReturn ts)) defs
  toLangType val = case val of
    IntVal {} -> IntType
    FloatVal {} -> FloatType
    StringVal {} -> StringType
    BoolVal {} -> BooleanType
    DictVal {} -> DictType
    DictKey {} -> DictKeyType
    Undefined {} -> UndefinedType
    TupleVal {} -> TupleType AnyType
    ListVal {} -> ListType AnyType
    DataVal cons name _ -> TypeConstructorType cons (toLangType name)
    s -> error $ "Not implemented" ++ show s

showTypeSig :: TypeSig -> String
showTypeSig TypeSig {typeSigName = name, typeSigIn = inn, typeSigReturn = rtrn} = "(TypeSig {typeSigName = " ++ show name ++ ", typeSigIn = " ++ show inn ++ ", typeSigReturn = " ++ show rtrn ++ "})"

joinCommaSep :: Show a => [a] -> String
joinCommaSep contents = intercalate ", " (map show contents)

joinCommaSepWithShowFun :: (a -> String) -> [a] -> String
joinCommaSepWithShowFun fun contents = intercalate ", " (map fun contents)
