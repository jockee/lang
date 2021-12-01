module TypeCheck where

import Control.Arrow
import Control.Exception
import Data.Aeson.Types (JSONPathElement (Key))
import Data.Data
import Data.Foldable (asum)
import Data.Hashable
import Data.List
import Data.List qualified as List
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord
import Debug.Trace
import Exceptions
import Types

instance Hashable Env where
  hashWithSalt k Env {envValues = v} = k `hashWithSalt` Map.keys v

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
patternMatch (PList _ []) (ListVal []) = True
patternMatch (PString str) (StringVal val) = str == val
patternMatch (PList _ [_]) (ListVal [_]) = True
patternMatch (PList _ _) _ = False
patternMatch (PDataConstructor exprName _) (DataVal _ valName _) = exprName == valName
patternMatch (ConsList bindings) (ListVal xs) = length xs >= length bindings - 1
patternMatch (PBool a) (BoolVal b) = a == b
patternMatch (PInteger e) (IntVal v) = e == v
patternMatch (PFloat e) (FloatVal v) = e == v
patternMatch _ _ = True

typeCheck :: Env -> LangType -> Expr -> Either String Env
typeCheck env _ (PTypeSig ts) = Right (typeSigToEnv env ts)
--
-- TODO: check return type in expression:
-- TODO: needs to recur down the ast
--  * do we need to step back up? only if a single expression can hold many expressions?
--  `a :: String -> Integer
--  `b :: Integer -> Integer
--  `b y = let x = a 1 in 2` --
typeCheck env expects (Binop Assign (Atom _ a) (Lambda ts args e)) = typeCheck env expects $ toExpr e
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
    [(_modules, DataConstructorDefinitionVal envTCons _)] -> tcons == envTCons
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
typeCheckMany = foldl fl (Right emptyEnv)
  where
    fl (Right env) ex = typeCheck env AnyType ex
    fl (Left err) _ = Left err

moduleToEnv :: Env -> String -> Env
moduleToEnv env name = env {inModule = inModule env ++ [name], withModules = withModules env ++ [name]}

typeSigToEnv :: Env -> TypeSig -> Env
typeSigToEnv env ts =
  case typeSigName ts of
    Just name -> env {typeSigs = Map.insert name ts (typeSigs env)}
    Nothing -> env

inScope :: Env -> String -> [([Module], Val)]
inScope env rawLookupKey = case Map.lookup key (envValues env) of
  Just envEntries -> fst $ foldr findLowest ([], 0) envEntries
  Nothing -> []
  where
    findLowest eE@EnvEntry {envEntryModules = modules', envEntryValue = val} (acc, bestScopeRank) =
      let scopeRank' = scopeRank eE
       in if scopeRank' > 0 && scopeRank' >= bestScopeRank
            then ((modules', val) : acc, scopeRank')
            else (acc, bestScopeRank)
    scopeRank EnvEntry {envEntryModules = modules', envEntryScope = scope} =
      if null modules' || matchesModules modules'
        then case scope of
          Nothing -> 1
          Just scope' -> maybe 0 (+ 2) (List.elemIndex scope' (envScopes env))
        else 0
    key = last namespaced
    modules = init namespaced
    namespaced = splitOn "." rawLookupKey
    matchesModules [] = True
    matchesModules modules' =
      not . null $ List.intersect modules' modules ++ List.intersect modules' (inModule env)

setScope :: Env -> Env
setScope env = trace ("SET scope") $ newEnv
  where
    newScope = show $ hash newEnv
    newEnv = env {envScopes = envScopes env ++ [newScope]}

unsetScope :: Env -> Env
unsetScope env = trace ("UNSET scope") $ newEnv
  where
    newEnv = env {envScopes = init $ envScopes env}

resetScope :: Env -> Env
resetScope env = env {envScopes = defaultEnvScopes}
