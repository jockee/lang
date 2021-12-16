{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Eval (evals, eval, evalsIn, extend, evalIn) where

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
import Extension
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
evalIn env (Block exprs) = foldl' (\acc ex -> acc `seq` evalIn env ex) (Undefined, env) exprsWithoutEndingNoop
  where
    exprsWithoutEndingNoop = reverse $ dropWhile (== PNoop) $ reverse exprs
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
        Just (pred, predDo) -> evalIn (extend env env pred condVal) predDo
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
    s -> error $ "DictAccess: " ++ show s
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
evalIn env (Binop MapPipe e1 e2) = evalIn env (App (App (Atom anyTypeSig "map") e1) e2)
evalIn env (Cons exprs) = evalIn env $ foldl1 foldFun exprs
  where
    foldFun acc ex = App (App (hfiLambda 2 "cons") acc) ex
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
evalIn env (Binop AddOrConcat e1 e2) =
  let (v2, _) = evalIn env e2
   in case v2 of
        FloatVal {} -> (v1 `plus` v2, env)
        IntVal {} -> (v1 `plus` v2, env)
        _ -> evalIn env (App (App (hfiLambda 2 "concat") v1) v2)
  where
    (v1, _) = evalIn env e1
    plus = evalOp Add
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

hfiFun :: Env -> Id -> Expr -> (Val, Env)
hfiFun env f argsList = case evaledArgsList of
  ListVal evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    fun :: Id -> [Val] -> Val
    fun "cons" (a : b : _) = case (a, b) of
      (a', ListVal bs) -> ListVal $ a' : bs
      (StringVal a', StringVal bs) -> StringVal $ T.append a' bs -- NOTE: this concats instead of conses
      s -> error $ "HFI cons" ++ show s
    fun "concat" (a : b : _) = case (a, b) of
      (ListVal a', ListVal bs) -> ListVal $ a' ++ bs
      (StringVal a', StringVal bs) -> StringVal $ T.append a' bs
      s -> error $ "HFI concat" ++ show s
    fun "downcase" ((StringVal s) : _) = StringVal $ T.toLower s
    fun "uppercase" ((StringVal s) : _) = StringVal $ T.toUpper s
    fun "toChars" ((StringVal s) : _) = ListVal $ map (StringVal . T.singleton) (T.unpack s)
    fun "dictToList" (dict : _) = case dict of
      (DictVal d) -> ListVal $ map (\(DictKey k, v) -> TupleVal [StringVal $ T.pack k, v]) (Map.toList d)
    fun "readFile" (StringVal path : _) = StringVal $ unsafePerformIO $ T.pack <$> readFile' (envLangPath env ++ T.unpack path)
    fun "writeFile" (StringVal path : StringVal body : _) = let !file = (unsafePerformIO $ writeFile (T.unpack path) (T.unpack body)) in StringVal body
    fun "sleep" (a : _) = unsafePerformIO (threadDelay 1000000 >> pure a)
    fun "httpRequest" (StringVal url : StringVal method : StringVal body : _) = unsafePerformIO $ HTTP.request (T.unpack method) (T.unpack url) [] (T.unpack body)
    fun "getArgs" _ = ListVal $ map (StringVal . T.pack) $ unsafePerformIO getArgs
    fun "printNoNewline" (a : _) = unsafePerformIO (putStr (showRaw a) >> pure a)
    fun "print" (a : _) = unsafePerformIO (putStrLn (showRaw a) >> pure a)
    fun "decodeJSON" ((StringVal s) : _) = jsonToVal $ BS.pack $ T.unpack s
    fun "encodeJSON" (a : _) = StringVal . T.pack . BS.unpack $ encode a
    fun "debug" (a : b : _) = unsafePerformIO (print a >> pure b)
    fun "debugEnv" (a : _) = pTrace (show env) a
    fun x r = error ("No such HFI " ++ show x ++ show r)
    evaledArgsList = fst $ evalIn env $ toExpr argsList

apply :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
apply env e1 e2 =
  case evalIn env e1 of
    (Pattern definitions, env') -> case List.filter (matchingDefinition env' passedArg) definitions of
      [] ->
        let FunctionVal _ TypeSig {typeSigName = tsn} _ _ = head definitions
         in case tsn of
              Just name -> error $ "Could not find matching function definition '" ++ name ++ "' - none matched criteria"
              Nothing -> error "Could not find matching anonymous function - none matched criteria"
      [FunctionVal lambdaEnv ts args e3] -> callFunction env' lambdaEnv ts args passedArg e3
      funs@((FunctionVal lambdaEnv ts args e3) : _) ->
        if functionFullyApplied args
          then callFullyAppliedFunction env' lambdaEnv ts args passedArg e3 -- if many matches, fully apply first
          else
            let (partiallyAppliedFuns, accEnv) = foldl' toFunVal ([], env') funs
                toFunVal (accFuns, accEnv) fun = case fun of
                  FunctionVal lambdaEnv' ts args e3 ->
                    let (val, env'') = callPartiallyAppliedFunction accEnv lambdaEnv' ts args passedArg e3
                     in (accFuns ++ [val], env'')
                  _ -> error "Non-function application"
             in (Pattern partiallyAppliedFuns, accEnv)
    (fun@(FunctionVal lambdaEnv ts args e3), env') ->
      if matchingDefinition env' passedArg fun
        then callFunction env' lambdaEnv ts args passedArg e3
        else error "Found no matching definition"
    (val, env) -> error ("Cannot apply value" ++ show val ++ " in env: " ++ show env)
  where
    (passedArg, _) = evalIn env $ toExpr e2
    functionFullyApplied remainingArgs = length remainingArgs == 1
    callFunction env lambdaEnv ts args passedArg e3 =
      if functionFullyApplied args
        then callFullyAppliedFunction env lambdaEnv ts args passedArg e3
        else callPartiallyAppliedFunction env lambdaEnv ts args passedArg e3

callFullyAppliedFunction :: Evaluatable e => Env -> LambdaEnv -> TypeSig -> ArgsList -> Val -> e -> (Val, Env)
callFullyAppliedFunction env lambdaEnv ts argsList val1 e3 =
  let envInCorrectModule =
        if null (typeSigName ts)
          then (env {inModule = typeSigModule ts})
          else env
      withLastArgExtended = extend envInCorrectModule lambdaEnv arg val1
      callWithScope = (mergeLambdaEnvIntoEnv env ts withLastArgExtended) {inModule = typeSigModule ts}
      (arg : remainingargs') = argsList
      (val, env'') = evalIn callWithScope (toExpr e3)
   in val `seq` (val, env)

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

extend :: (Evaluatable e, HasBindings h) => Env -> h -> Expr -> e -> h
extend env bindable argExpr e = extend' env bindable argExpr val
  where
    !val = fst . evalIn env $ toExpr e

eval :: Expr -> Val
eval = fst . evalIn emptyEnv

evalsIn :: Env -> [Expr] -> (Val, Env)
evalsIn env = foldl' fl (Undefined, env)
  where
    fl (!_val, env) ex = evalIn (resetScope env) ex

evals :: [Expr] -> Val
evals exprs = fst $ evalsIn emptyEnv exprs

traceUnlessStdLib :: forall a. Env -> String -> a -> a
traceUnlessStdLib env s a =
  if envInStdLib env
    then a
    else trace s a
