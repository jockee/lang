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
import Syntax

instance Hashable Env where
  hashWithSalt k Env {envValues = v} = k `hashWithSalt` Map.keys v

typeCheck :: Env -> LangType -> Expr -> (Either String Env)
typeCheck env _ (NamedTypeSig ts) = Right (typeSigToEnv env ts)
--
-- TODO: check return type in expression:
-- TODO: needs to recur down the ast
--  * do we need to step back up? only if a single expression can hold many expressions?
--  `a :: String -> Integer
--  `b :: Integer -> Integer
--  `b y = let x = a 1 in 2` --
typeCheck env expects (Binop Assign (Atom _ a) (Lambda ts args e)) = typeCheck env expects $ toExpr e
-- typeCheck env (Binop Assign (Atom _ a) e) = trace ("calling f with x = " ++ show env) $ undefined
-- let env'' = extend env' a AnyType v
--     (value, env') = evalIn env v
--  in (value, env'')
-- typeCheck env expects (App (Atom _ a) e) = undefined -- runFun (withScope env) e1 e2
typeCheck env StringType (PString n) = (Right env)
typeCheck env FloatType (PFloat n) = (Right env)
typeCheck env IntType (PInteger n) = (Right env)
typeCheck env AnyType (PInteger n) = (Right env)
typeCheck env expects got = error ("Expected " ++ show expects ++ ", but got " ++ show got)

-- typeCheck env expects got = Left ("Expected " ++ show expects ++ ", but got " ++ show got)

expectedType :: Env -> TypeSig -> Int -> LangType
expectedType env ts argsRemaining =
  case typeSigName ts of
    Just name -> maybe AnyType typeAtPos (inTypes name)
    Nothing -> AnyType -- not named, so for the time being not typed
  where
    typeAtPos x =
      let types = typeSigIn x
       in types !! (length types - argsRemaining)
    inTypes name = Map.lookup name (typeSigs env)

-- typeCheck env (PIf (PBool True) t _) = typeCheck env t
-- typeCheck env (PIf (PBool False) _ f) = typeCheck env f
-- typeCheck env (PIf condition ifTrue ifFalse) =
--   let (val, env') = typeCheck env condition
--    in if val == Boolean True then typeCheck env' ifTrue else typeCheck env' ifFalse
-- typeCheck env (InternalFunction f args) = internalFunction env f args
-- typeCheck env (App e1 e2) = runFun (withScope env) e1 e2
-- typeCheck env (Atom ts atomId) = case inScope env atomId of
--   Just a -> (a, env)
--   Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
-- typeCheck env (PDictUpdate baseDict updateDict) =
--   let (DictVald1) = fst $ typeCheck env baseDict
--       (DictVald2) = fst $ typeCheck env updateDict
--    in (DictVal$ Map.union d2 d1, env)
-- typeCheck env (DictAccess k dict) =
--   let (DictValm) = fst $ typeCheck env dict
--       kv = fst $ typeCheck env k
--    in (fromJust (Map.lookup kv m), env)
-- typeCheck env (PDictKey k) = (DictKey k, env)
-- typeCheck env (PDict ts pairs) =
--   let fn (k, v) = (fst $ typeCheck env k, fst $ typeCheck env v)
--    in (DictVal$ Map.fromList $ map fn pairs, env)
-- typeCheck env (PRange ts lBoundExp uBoundExp) =
--   let lBound = fst $ typeCheck env lBoundExp
--       uBound = fst $ typeCheck env uBoundExp
--    in case (lBound, uBound) of
--         (IntVal l, IntVal u) -> (List $ map IntVal [l .. u], env)
--         _ -> error "Invalid range"
-- typeCheck env (PList ts es) = (List $ map (fst . typeCheck env) es, env)
-- typeCheck env (PTuple ts es) = (Tuple $ map (fst . typeCheck env) es, env)
-- typeCheck env (PBool n) = (Boolean n, env)
-- typeCheck env (Binop Assign (PTuple ts1 bindings) (PTuple ts2 vs)) =
--   let evaledValues = map (fst . typeCheck env) vs
--       newEnv = extendWithTuple env bindings evaledValues
--    in if length bindings /= length evaledValues
--         then throw $ EvalException "Destructuring failed. Mismatched parameter count"
--         else (Tuple evaledValues, newEnv)
-- typeCheck env (Binop Concat e1 e2) =
--   let (List xs, _) = typeCheck env e1
--       (List ys, _) = typeCheck env e2
--       e = error "Invalid"
--    in (List $ xs ++ ys, env)
-- typeCheck env (Binop Pipe e1 e2) = runFun (withScope env) e2 e1
-- typeCheck env (Binop Assign (Atom ts a) v) =
--   let env'' = extend env' a AnyType v
--       (value, env') = typeCheck env v
--    in (value, env'')
-- typeCheck env (Binop op e1 e2) =
--   let (v1, _) = typeCheck env e1
--       (v2, _) = typeCheck env e2
--       x = evalOp op
--    in (v1 `x` v2, env)
-- typeCheck env (Cmp op e1 e2) =
--   let (v1, _) = typeCheck env e1
--       (v2, _) = typeCheck env e2
--       x = cmpOp op
--    in (v1 `x` v2, env)
-- typeCheck env PNoop = (Undefined, env)
-- typeCheck _ a = error $ "failed to find match in typeCheck" ++ show a

typeCheckMany :: [Expr] -> Either String Env
typeCheckMany exprs = foldl fl (Right emptyEnv) exprs
  where
    fl (Right env) ex = typeCheck env AnyType ex
    fl (Left err) _ = Left err

typeSigToEnv :: Env -> TypeSig -> Env
typeSigToEnv env ts =
  case typeSigName ts of
    Just name -> env {typeSigs = Map.insert name ts (typeSigs env)}
    Nothing -> env

inScope :: Env -> String -> Maybe [Val]
inScope env atomId =
  -- trace ("SCOPEKEYS " ++ show scopeKeys ++ show env) $
  asum $ map (\k -> Map.lookup k (envValues env)) scopeKeys
  where
    scopeKeys = reverse $ map (\k -> k ++ ":" ++ atomId) $ envScopes env

withScope :: Env -> Env
withScope env = newEnv
  where
    newScope = show $ hash newEnv
    newEnv = env {envScopes = List.nub $ envScopes env ++ [newScope]}

emptyEnv :: Env
emptyEnv = Env {envValues = Map.empty, envScopes = defaultEnvScopes, typeSigs = Map.empty}

defaultEnvScopes :: [String]
defaultEnvScopes = ["global"]

resetScope :: Env -> Env
resetScope env = env {envScopes = defaultEnvScopes}
