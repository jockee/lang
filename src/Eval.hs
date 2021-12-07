{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Eval where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (foldl')
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Debug.Pretty.Simple
import Debug.Trace
import Exceptions
import Http qualified as HTTP
import Parser (parseExprs)
import System.Environment
import System.IO.Extra (readFile')
import System.IO.Unsafe
import TypeCheck
import Types
import Util

evalIn :: Env -> Expr -> (Val, Env)
evalIn env (Evaluated val) = (val, env)
evalIn env (PatternExpr modVals) = (Pattern modVals, env)
evalIn env (PImport stringExpr) = case fst $ evalIn env stringExpr of
  (StringVal filePath) ->
    let contents = unsafePerformIO $ readFile' (envLangPath env ++ T.unpack filePath)
        parsed = parseExprs $ strip contents
     in evalsIn env parsed
evalIn env (PTrait name types funs) =
  ( Undefined,
    snd $ evalsIn (extendWithTrait env env name types) funs
  )
evalIn env (PImplementation trait _for defs) = (Undefined, extendWithImplementation env env trait defs)
evalIn env (Module name ex) =
  let (val, env') = evalsIn (moduleToEnv env name) ex
   in (val, env' {inModule = inModule env})
evalIn env (PDataDefinition name constructors) = (Undefined, extendWithDataDefinition env env name constructors)
evalIn env (PDataConstructor name exprArgs) =
  let evaledArgs = map (fst . evalIn env) exprArgs
   in case inScope env name of
        [DataConstructorDefinitionVal dtype argVals] ->
          if length exprArgs > length argVals
            then error $ "Too many arguments to value constructor " ++ show name
            else case List.find (\(_i, val, ex) -> not $ concreteTypesMatch env (toLangType val) (toLangType ex)) (zip3 [1 ..] argVals exprArgs) of
              Just (i, ex, val) -> error $ "Mismatched types. Wanted " ++ show (toLangType val) ++ ", got: " ++ show (toLangType ex) ++ " in position " ++ show (i + 1)
              Nothing -> (DataVal dtype name evaledArgs, env)
        [] -> error $ "No such constructor found: " ++ show name
        s -> error $ "Constructor not implemented: " ++ show s
evalIn env (PTypeSig ts) = (Undefined, typeSigToEnv env ts)
evalIn env (PCase ts cond cases) =
  let condVal = fst $ evalIn env cond
      findFun (pred, predDo) = matchingDefinition env condVal (FunctionVal emptyLambdaEnv ts [pred] predDo)
   in case List.find findFun cases of
        Just (pred, predDo) -> evalIn (extend env env pred cond) predDo
evalIn env (Lambda lambdaEnv ts args e) =
  let lambdaEnv' =
        if null (typeSigName ts)
          then LambdaEnv {lambdaEnvBindings = Map.unionWith (++) (localEnvBindings env) (lambdaEnvBindings lambdaEnv)}
          else lambdaEnv
   in (FunctionVal lambdaEnv' (ts {typeSigModule = inModule env}) args e, env)
evalIn env (HFI f args) = hfiFun env f args
evalIn env (App e1 e2) = apply env e1 e2
evalIn env (Atom _ts atomId) = case inScope env atomId of
  [definition] -> (definition, env)
  [] -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
  definitions -> case last definitions of
    FunctionVal {} -> (Pattern definitions, env)
    lastDef -> (lastDef, env)
evalIn env (PInterpolatedString parts) =
  let fn x = case fst $ evalIn env x of
        StringVal s -> s
        o -> T.pack $ prettyVal o
   in (StringVal $ T.intercalate "" $ map fn parts, env)
evalIn env (PString n) = (StringVal n, env)
evalIn env (PFloat n) = (FloatVal n, env)
evalIn env (PInteger n) = (IntVal n, env)
evalIn env (PDictUpdate baseDict updateDict) =
  let (DictVal d1) = fst $ evalIn env baseDict
      (DictVal d2) = fst $ evalIn env updateDict
   in (DictVal $ Map.union d2 d1, env)
evalIn env (DictAccess k dict) =
  case fst $ evalIn env dict of
    (DictVal m) ->
      let kv = fst $ evalIn env k
       in (fromJust (Map.lookup kv m), env)
    s -> error $ show s
-- evalIn env (PDictKeyLookup k) =
--   evalIn env $
--     App
--       ( Lambda
--           emptyLambdaEnv
--           anyTypeSig
--           [Atom anyTypeSig "keyInPDictKeyLookup", Atom anyTypeSig "dictInPDictKeyLookup"]
--           (DictAccess (Atom anyTypeSig "InPDictKeyLookupkey") (Atom anyTypeSig "dictInPDictKeyLookup"))
--       )
--       (DictKey k)
evalIn env (PDictKey k) = (DictKey k, env)
evalIn env (PDict _ts pairs) =
  let fn (k, v) =
        let key = case fst $ evalIn env k of
              (DictKey s) -> DictKey s
              (StringVal s) -> DictKey $ T.unpack s
              _ -> error "Non-string dict key"
            val = fst $ evalIn env v
         in (key, val)
   in (DictVal $ Map.fromList $ map fn pairs, env)
evalIn env (PRange _ts lBoundExp uBoundExp) =
  let lBound = fst $ evalIn env lBoundExp
      uBound = fst $ evalIn env uBoundExp
   in case (lBound, uBound) of
        (IntVal l, IntVal u) -> (ListVal $ map IntVal [l .. u], env)
        _ -> error "Invalid range"
evalIn env (PList _ts es) = (ListVal $ map (fst . evalIn env) es, env)
evalIn env (PTuple _ts es) = (TupleVal $ map (fst . evalIn env) es, env)
evalIn env (PBool n) = (BoolVal n, env)
evalIn env (Binop Pipe e1 e2) = evalIn env (App e2 e1)
evalIn env (Binop FmapPipe e1 e2) = evalIn env (App (App (Atom anyTypeSig "fmap") e1) e2)
evalIn env (Binop Cons e1 e2) = evalIn env (App (App (hfiLambda 2 "cons") e1) e2)
evalIn env (Binop Concat e1 e2) = evalIn env (App (App (hfiLambda 2 "concat") e1) e2)
evalIn env (Binop Assign atom@(Atom _ts funName) (Lambda lambdaEnv ts args e)) =
  let ts' = fromMaybe ts $ typeFromEnv env funName
      value = fst $ evalIn (mergeLambdaEnvIntoEnv env ts' lambdaEnv) (Lambda lambdaEnv ts' args e)
      env' = extend env env atom value
   in if length args /= length (typeSigIn ts')
        then error $ "Function definition argument count differs from typesig argument count for function " ++ show funName
        else (value, env')
evalIn env (Binop Assign expr v) =
  let !env' = extend env env expr v
      (value, _) = evalIn env v
   in (value, env')
evalIn env (Unaryop Not e) =
  case fst $ evalIn env e of
    (BoolVal b) -> (BoolVal $ not b, env)
evalIn env (Unaryop op e) =
  let (v, _) = evalIn env e
   in (evalUnOp op v, env)
evalIn env (Binop op e1 e2) =
  let (v1, _) = evalIn env e1
      (v2, _) = evalIn env e2
      x = evalOp op
   in (v1 `x` v2, env)
evalIn env (Cmp op e1 e2) =
  let (v1, _) = evalIn env e1
      (v2, _) = evalIn env e2
      x = cmpOp op
   in (v1 `x` v2, env)
evalIn env PNoop = (Undefined, env)
evalIn _ a = error $ "failed to find match in evalIn: " ++ show a

hfiLambda :: Int -> Id -> Expr
hfiLambda argCount name = Lambda emptyLambdaEnv ts args $ HFI name $ PList anyTypeSig args
  where
    ts = anyTypeSig {typeSigIn = replicate argCount AnyType}
    args = take argCount $ map (\x -> Atom anyTypeSig (x : (name ++ "LambdaArg"))) ['a' ..]

extendWithDataDefinition :: HasBindings h => Env -> h -> String -> [ConstructorWithArgs] -> h
extendWithDataDefinition env bindable typeCons = foldl' foldFun bindable
  where
    foldFun accBindable (vc, args) = extend env accBindable (Atom anyTypeSig vc) (DataConstructorDefinitionVal typeCons args)

extendWithListable :: HasBindings h => Env -> h -> [Expr] -> [Val] -> h
extendWithListable env bindable bindings vs
  | length bindings /= length vs = throw $ EvalException "Destructuring failed. Mismatched parameter count"
  | otherwise = foldl' foldFun bindable (zip bindings vs)
  where
    foldFun accBindable (binding, val) = extend env accBindable binding val

extendWithTrait :: HasBindings h => Env -> h -> String -> [Expr] -> h
extendWithTrait env bindable name defs = extend env bindable (Atom anyTypeSig name) $ TraitVal name defs

extendWithImplementation :: HasBindings h => Env -> h -> Trait -> [Expr] -> h
extendWithImplementation env bindable trait = foldl' foldFun bindable
  where
    foldFun accBindable def = case def of
      (Binop Assign atom@(Atom _ts funName) (Lambda lambdaEnv ts args e)) ->
        let newTS = updateTS ts funName
         in extend env accBindable atom (Lambda lambdaEnv newTS args e)
      (Binop Assign atom val) -> extend env accBindable atom val
    updateTS oldTS funName =
      let ts = fromMaybe oldTS (tsFromTraitDef funName)
       in ts {typeSigTraitBinding = typeSigTraitBinding oldTS, typeSigImplementationBinding = typeSigImplementationBinding oldTS}
    tsFromTraitDef funName = case inScope env trait of
      [TraitVal _ defs] -> case List.find (findDef funName) defs of
        Just (PTypeSig ts) -> Just ts
        _ -> Nothing
      _ -> error $ "Got more than one trait definition for " ++ show trait
    findDef funName (PTypeSig ts) = typeSigName ts == Just funName

-- let {a: b} = {a: 1} in
-- let {a: 1, b: c} = {a: 1, b: 2} in
-- let {a: b, ...} = {a: 1, b: 2, c: 2} in
extendWithDict :: HasBindings h => Env -> h -> [(Expr, Expr)] -> Map.Map Val Val -> h
extendWithDict env bindable exprPairs valMap = foldl' foldFun bindable exprPairs
  where
    foldFun accBindable expr = case expr of
      (PDictKey a, atom@(Atom _ts atomId)) -> case Map.lookup (DictKey a) valMap of
        Just val -> extend env bindable atom val
        Nothing -> error "Dictionary doesn't contain key"
      (PDictKey a, expr) ->
        let evaledExpr = fst $ evalIn env expr
         in ( case Map.lookup (DictKey a) valMap of
                Just val -> if val == evaledExpr then accBindable else error "Dictionary not matching pattern"
                Nothing -> error "Dictionary doesn't contain key"
            )

extendWithConsList :: HasBindings h => Env -> h -> [String] -> [Val] -> h
extendWithConsList env bindable bindings vs
  | length bindings - 1 > length vs = error $ "Too many bindings. Wanted " ++ show (length bindings) ++ ", but got " ++ show (length vs)
  | otherwise =
    let llast = (last bindings, ListVal $ drop (length bindings - 1) vs)
        linit = zip (take (length bindings - 1) bindings) vs
     in foldFun llast (foldr foldFun bindable linit)
  where
    foldFun (binding, val) accBindable = extend env accBindable (Atom anyTypeSig binding) val

extendWithDataConstructor :: HasBindings h => Env -> h -> [Expr] -> [Val] -> h
extendWithDataConstructor env bindable exprs vals = foldl' foldFun bindable (zip exprs vals)
  where
    foldFun accEnv (expr, val) = extend env accEnv expr val

extend :: (Evaluatable e, HasBindings h) => Env -> h -> Expr -> e -> h
extend env bindable argExpr e = case argExpr of
  (Atom _ id) -> extendAtom env bindable id val
  (PDataConstructor _consName exprs) -> case val of
    (DataVal _dtype _name vals) -> extendWithDataConstructor env bindable exprs vals
    s -> error $ "Non-value-cons value received for value cons destructuring" ++ show s
  (PDict _ts exprPairs) -> case val of
    (DictVal valMap) -> extendWithDict env bindable exprPairs valMap
    _ -> error "Non-dict value received for dict destructuring"
  (ConsList bindings) -> case val of
    (ListVal vals) -> extendWithConsList env bindable bindings vals
    _ -> error "Non-list value received for cons destructuring"
  (PTuple _ts bindings) -> case val of
    (TupleVal vals) -> extendWithListable env bindable bindings vals
    s -> error $ "Non-tuple value " ++ show s ++ " received for tuple destructuring"
  (PList _ts bindings) -> case val of
    (ListVal vals) -> extendWithListable env bindable bindings vals
    _ -> error "Non-list value received for list destructuring"
  _ -> bindable
  where
    !val = fst $ evalIn env $ toExpr e

extendAtom :: (Evaluatable e, HasBindings h) => Env -> h -> String -> e -> h
extendAtom _ bindable "_" _ = bindable
extendAtom env bindable id e =
  setBindings bindable withUpdatedScope
  where
    withUpdatedScope = Map.insertWith insertFun id [envEntry] (getBindings bindable)
    insertFun = if id == "@" then const else flip (++)
    envEntry =
      EnvEntry
        { envEntryValue = fst $ evalIn env $ toExpr e,
          envEntryModule = inModule env
        }

hfiFun :: Evaluatable e => Env -> Id -> e -> (Val, Env)
hfiFun env f argsList = case evaledArgsList of
  ListVal evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    fun :: Id -> [Val] -> Val
    fun "cons" (a : b : _) = case (a, b) of
      (a', ListVal bs) -> ListVal $ a' : bs
      (StringVal a', StringVal bs) -> StringVal $ T.append a' bs -- NOTE: this concats instead of conses
      s -> error $ show s
      (a', ListVal bs) -> ListVal $ a' : bs
      (StringVal a', StringVal bs) -> StringVal $ T.append a' bs -- NOTE: this concats instead of conses
      s -> error $ show s
    fun "concat" (a : b : _) = case (a, b) of
      (ListVal a', ListVal bs) -> ListVal $ a' ++ bs
      (StringVal a', StringVal bs) -> StringVal $ T.append a' bs
      s -> error $ show s
    fun "toChars" ((StringVal s) : _) = ListVal $ map (StringVal . T.singleton) (T.unpack s)
    fun "dictToList" (dict : _) = case dict of
      (DictVal d) -> ListVal $ map (\(DictKey k, v) -> TupleVal [StringVal $ T.pack k, v]) (Map.toList d)
    fun "readFile" (StringVal path : _) = StringVal $ unsafePerformIO $ T.pack <$> readFile' (envLangPath env ++ T.unpack path)
    fun "writeFile" (StringVal path : StringVal body : _) = let !file = (unsafePerformIO $ writeFile (T.unpack path) (T.unpack body)) in StringVal body
    fun "sleep" (a : _) = unsafePerformIO (threadDelay 1000000 >> pure a)
    fun "httpRequest" (StringVal url : StringVal method : StringVal body : _) = unsafePerformIO $ HTTP.request (T.unpack method) (T.unpack url) [] (T.unpack body)
    fun "getArgs" _ = ListVal $ map (StringVal . T.pack) $ unsafePerformIO getArgs
    fun "print" (a : _) = unsafePerformIO (putStrLn (showRaw a) >> pure a)
    fun "decodeJSON" ((StringVal s) : _) = jsonToVal $ BS.pack $ T.unpack s
    fun "encodeJSON" (a : _) = StringVal . T.pack . BS.unpack $ encode a
    fun "debug" (a : b : _) = unsafePerformIO (print a >> pure b)
    fun "debugEnv" (a : _) = pTrace (show env) a
    fun x r = error ("No such HFI " ++ show x ++ show r)
    funToExpr (FunctionVal lambdaEnv ts args e) = Lambda lambdaEnv ts args e
    funToExpr (Pattern defs) = PatternExpr defs
    funToExpr r = error $ show r
    evaledArgsList = fst $ evalIn env $ toExpr argsList

apply :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
apply env e1 e2 =
  case evalIn env e1 of
    (Pattern definitions, env') -> case List.filter (matchingDefinition env' passedArg) definitions of
      [] -> error "Could not find matching function definition - none matched criteria"
      [FunctionVal lambdaEnv ts args e3] -> callFunction env' lambdaEnv ts args e2 e3
      funs@((FunctionVal lambdaEnv ts args e3) : _) ->
        if functionFullyApplied args
          then callFullyAppliedFunction env' lambdaEnv ts args e2 e3
          else
            let (appliedFuns, accEnv) = foldl' toFunVal ([], env') funs
                toFunVal (accFuns, accEnv) fun = case fun of
                  FunctionVal lambdaEnv' ts args e3 ->
                    let (val, env'') = callPartiallyAppliedFunction accEnv lambdaEnv' ts args e2 e3
                     in (accFuns ++ [val], env'')
                  _ -> error "Non-function application"
             in (Pattern appliedFuns, accEnv)
    (FunctionVal lambdaEnv ts args e3, env') -> callFunction env' lambdaEnv ts args e2 e3
    (val, env) -> error ("Cannot apply value" ++ show val ++ " in env: " ++ show env)
  where
    (passedArg, _) = evalIn env $ toExpr e2
    functionFullyApplied args = length args == 1
    callFunction env lambdaEnv ts args e2 e3 =
      if functionFullyApplied args
        then callFullyAppliedFunction env lambdaEnv ts args e2 e3
        else callPartiallyAppliedFunction env lambdaEnv ts args e2 e3

callFullyAppliedFunction :: (Evaluatable e1, Evaluatable e2) => Env -> LambdaEnv -> TypeSig -> ArgsList -> e1 -> e2 -> (Val, Env)
callFullyAppliedFunction env lambdaEnv ts argsList e2 e3 =
  let envInCorrectModule =
        if null (typeSigName ts)
          then (env {inModule = typeSigModule ts})
          else env
      withLastArgExtended = extend envInCorrectModule lambdaEnv arg e2
      callWithScope = (mergeLambdaEnvIntoEnv env ts withLastArgExtended) {inModule = typeSigModule ts}
      (arg : remainingargs') = argsList
      (val, env'') = evalIn callWithScope (toExpr e3)
   in (val, env)

mergeLambdaEnvIntoEnv :: Env -> TypeSig -> LambdaEnv -> Env
mergeLambdaEnvIntoEnv env ts lambdaEnv =
  if null (typeSigName ts)
    then env {envLambdaEnvs = envLambdaEnvs env ++ [lambdaEnv]}
    else env {envLambdaEnvs = [lambdaEnv]}

callPartiallyAppliedFunction :: (Evaluatable e1, Evaluatable e2) => Env -> LambdaEnv -> TypeSig -> ArgsList -> e1 -> e2 -> (Val, Env)
callPartiallyAppliedFunction env lambdaEnv ts argsList expr funExpr =
  evalIn env (Lambda lambdaEnv' ts remainingArgs' funExpr)
  where
    lambdaEnv' = extend env lambdaEnv arg expr
    (arg : remainingArgs') = argsList

eval :: Expr -> Val
eval = fst . evalIn emptyEnv

evalsIn :: Env -> [Expr] -> (Val, Env)
evalsIn env = foldl' fl (Undefined, env)
  where
    fl (!_val, env) ex = evalIn (resetScope env) ex

evals :: [Expr] -> Val
evals exprs = fst $ evalsIn emptyEnv exprs
