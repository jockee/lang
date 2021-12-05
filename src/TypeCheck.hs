{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeCheck where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Types

matchingDefinition :: Env -> Val -> Val -> Bool
matchingDefinition env passedArg (FunctionVal ts _ args@(expectedArgExp : _) _) =
  typesMatch env (Right (ts, Just $ length args)) passedArg
    && patternMatch expectedArgExp passedArg
matchingDefinition _ _ _ = False

typesMatch :: Env -> Either LangType (TypeSig, Maybe Int) -> Val -> Bool
typesMatch env (Right (ts, argsLength)) passedArg =
  concreteTypesMatch env expectedType' passedArgType
    && implementationBindingMatches (fmap (typeAtPos ts) argsLength) (typeSigImplementationBinding ts) passedArg
  where
    passedArgType = toLangType passedArg
    expectedType' = case expectedType env ts (fromMaybe 0 argsLength) of
      (TraitVariableType trait _) -> case typeSigImplementationBinding ts of
        Just binding -> case typeSigTraitBinding ts of
          Just traitBinding ->
            if trait == traitBinding
              then toLangType binding
              else error ("Mismatching trait. Expected: " ++ trait ++ ", got: " ++ show (typeSigTraitBinding ts))
        Nothing -> error "Got implementation binding, but no trait binding"
      lType -> lType
typesMatch env (Left expType) passedArg =
  concreteTypesMatch env expType (toLangType passedArg)

implementationBindingMatches :: Maybe LangType -> Maybe String -> Val -> Bool
implementationBindingMatches (Just (TraitVariableType _trait _)) implBinding passedArg =
  maybe True (`matcher` passedArg) implBinding
  where
    matcher binding (DataVal dConsFromPassed _ _) = dConsFromPassed == binding
    matcher binding (FunctionVal TypeSig {typeSigImplementationBinding = tsib} _ _ _) = tsib == Just binding
    matcher s t
      | toLangType s == toLangType t = True
      | otherwise = False
implementationBindingMatches _ _ _ = True

patternMatch :: Expr -> Val -> Bool
patternMatch (PList _ []) (ListVal []) =  True
patternMatch (PString str) (StringVal val) = str == val
patternMatch (PList _ [_]) s@ (ListVal [_]) =  True
patternMatch (PList _ _) _ = False
patternMatch (PDataConstructor exprName _) (DataVal _ valName _) = exprName == valName
patternMatch (ConsList bindings) (ListVal xs) = length xs >= length bindings - 1
patternMatch (PBool a) (BoolVal b) = a == b
patternMatch (PInteger e) (IntVal v) = e == v
patternMatch (PFloat e) (FloatVal v) = e == v
patternMatch a b =  True

typeCheck :: Env -> LangType -> Expr -> Either String Env
typeCheck env _ (PTypeSig ts) = Right (typeSigToEnv env ts)
--
-- TODO: check return type in expression:
-- TODO: needs to recur down the ast
--  * do we need to step back up? only if a single expression can hold many expressions?
--  `a :: String -> Integer
--  `b :: Integer -> Integer
--  `b y = let x = a 1 in 2` --
typeCheck env expects (Binop Assign (Atom _ a) (Lambda _ ts args e)) = typeCheck env expects $ toExpr e
typeCheck env StringType (PString n) = Right env
typeCheck env FloatType (PFloat n) = Right env
typeCheck env IntType (PInteger n) = Right env
typeCheck env AnyType (PInteger n) = Right env
typeCheck env expects got = error ("Expected " ++ show expects ++ ", but got " ++ show got)

-- XXX: shouldn't be using env, as this can't statically typecheck? maybe typechecking creates its
-- XXX: own env which has the same information for types
concreteTypesMatch :: Env -> LangType -> LangType -> Bool
concreteTypesMatch env (DataConstructorType dcons) (TypeConstructorType tcons _) =
  case inScope env dcons of
    [DataConstructorDefinitionVal envTCons _] -> tcons == envTCons
    _ -> error $ "Got more than one definition for " ++ show dcons
concreteTypesMatch env (FunctionType expInArgs expRtrn) (FunctionType gotInArgs gotRtrn) =
  all (uncurry (concreteTypesMatch env)) (zip expInArgs gotInArgs)
    && length expInArgs == length gotInArgs
    && concreteTypesMatch env expRtrn gotRtrn
concreteTypesMatch _ AnyType _ = True
concreteTypesMatch _ a b = a == b

expectedType :: Env -> TypeSig -> Int -> LangType
expectedType env ts argsRemaining =
  case typeSigName ts of
    Just name ->
      if null $ typeSigIn ts
        then maybe AnyType (`typeAtPos` argsRemaining) (typeFromEnv env name)
        else typeAtPos ts argsRemaining
    Nothing -> AnyType -- not named, so for the time being not typed
  where

typeFromEnv :: Env -> String -> Maybe TypeSig
typeFromEnv env name = Map.lookup name (typeSigs env)

typeAtPos :: TypeSig -> Int -> LangType
typeAtPos ts argsRemaining =
  let types = typeSigIn ts
   in types !! (length types - argsRemaining)

typeCheckMany :: [Expr] -> Either String Env
typeCheckMany = foldl' fl (Right emptyEnv)
  where
    fl (Right env) ex = typeCheck env AnyType ex
    fl (Left err) _ = Left err

moduleToEnv :: Env -> String -> Env
moduleToEnv env name = env {inModule = Just name, includedModules = includedModules env ++ [name]}

typeSigToEnv :: Env -> TypeSig -> Env
typeSigToEnv env ts =
  case typeSigName ts of
    Just name -> env {typeSigs = Map.insert name ts (typeSigs env)}
    Nothing -> env

inScope :: Env -> String -> [Val]
inScope env rawLookupKey = inScope' $ allBindings
  where
    allBindings = foldl' (Map.unionWith (++)) Map.empty $ stdLibBindings env : envBindings env : map lambdaEnvBindings (envLambdaEnvs env)
    inScope' scopeMap = case Map.lookup key scopeMap of
      Nothing -> []
      Just eEs -> map envEntryValue $ filter matchesModules eEs
    key = last namespaced
    calledWithModules = init namespaced
    namespaced = splitOn "." rawLookupKey
    matchesModules EnvEntry {envEntryModule = Just module'} =
      Just module' == inModule env
        || module' `elem` includedModules env && module' `elem` calledWithModules
    matchesModules _ = True

availableBindings :: Env -> Map.Map String [EnvEntry]
availableBindings env = Map.foldlWithKey foldFun Map.empty allBindings
  where
    foldFun acc k vs = case filter matchesModules vs of
      [] -> acc
      matches -> Map.insert k matches acc
    allBindings = foldl' (Map.unionWith (++)) Map.empty $ envBindings env : map lambdaEnvBindings (envLambdaEnvs env)
    matchesModules EnvEntry {envEntryModule = Just module'} = Just module' == inModule env
    matchesModules _ = True

resetScope :: Env -> Env
resetScope env = env {envLambdaEnvs = []}
