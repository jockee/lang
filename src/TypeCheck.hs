module TypeCheck where

import Control.Exception
import Data.Data
import Data.Foldable (asum)
import Data.Hashable
import Data.List
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
import Types

instance Hashable Env where
  hashWithSalt k Env {envValues = v} = k `hashWithSalt` Map.keys v

typeCheck :: Env -> LangType -> Expr -> (Either String Env)
typeCheck env _ (PTypeSig ts) = Right (typeSigToEnv env ts)
--
-- TODO: check return type in expression:
-- TODO: needs to recur down the ast
--  * do we need to step back up? only if a single expression can hold many expressions?
--  `a :: String -> Integer
--  `b :: Integer -> Integer
--  `b y = let x = a 1 in 2` --
typeCheck env expects (Binop Assign (Atom _ a) (Lambda ts args e)) = typeCheck env expects $ toExpr e
typeCheck env StringType (PString n) = (Right env)
typeCheck env FloatType (PFloat n) = (Right env)
typeCheck env IntType (PInteger n) = (Right env)
typeCheck env AnyType (PInteger n) = (Right env)
typeCheck env expects got = error ("Expected " ++ show expects ++ ", but got " ++ show got)

-- XXX: shouldn't be using env, as this can't statically typecheck? maybe typechecking creates its
-- XXX: own env which has the same information for types
concreteTypesMatch :: Env -> LangType -> LangType -> Bool
concreteTypesMatch env (DataConstructorType dcons) (TypeConstructorType tcons _) = case inScope env dcons of
  Just [DataConstructorDefinitionVal envTCons _] -> tcons == envTCons
  _ -> error $ "Got more than one definition for " ++ show dcons
concreteTypesMatch _ AnyType _ = True
concreteTypesMatch _ (TraitVariableType {}) (FunctionType {}) = True -- NOTE: make sure functions match on arity and types
concreteTypesMatch _ (FunctionType {}) (FunctionType {}) = True -- NOTE: make sure functions match on arity and types
concreteTypesMatch _ _ AnyType = True -- XXX: in reality the value-side giving an AnyType should probably be an exception
concreteTypesMatch _ a b = a == b
concreteTypesMatch _ a b = False

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
typeCheckMany exprs = foldl fl (Right emptyEnv) exprs
  where
    fl (Right env) ex = typeCheck env AnyType ex
    fl (Left err) _ = Left err

moduleToEnv :: Env -> String -> Env
moduleToEnv env name = env {withModules = withModules env ++ [name]}

typeSigToEnv :: Env -> TypeSig -> Env
typeSigToEnv env ts =
  case typeSigName ts of
    Just name -> env {typeSigs = Map.insert name ts (typeSigs env)}
    Nothing -> env

inScope :: Env -> String -> Maybe [Val]
inScope env lookupKey =
  asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = reverse $ map (\k -> k ++ ":" ++ lookupKey) $ envScopes env

setScope :: Env -> Env
setScope env = newEnv
  where
    newScope = show $ hash newEnv
    newEnv = env {envScopes = List.nub $ envScopes env ++ [newScope]}

resetScope :: Env -> Env
resetScope env = env {envScopes = defaultEnvScopes}
