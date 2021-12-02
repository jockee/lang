{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eval where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (foldl')
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
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
    let contents = unsafePerformIO $ readFile' (envLangPath env ++ filePath)
        parsed = parseExprs $ strip contents
     in evalsIn env parsed
evalIn env (PTrait name types funs) =
  ( Undefined,
    snd $ evalsIn (extendWithTrait env name types) funs
  )
evalIn env (PImplementation trait _for defs) = (Undefined, extendWithImplementation env trait defs)
evalIn env (Module name ex) =
  let (val, env') = evalsIn (moduleToEnv env name) ex
   in (val, env' {inModule = inModule env})
evalIn env (PDataDefinition name constructors) = (Undefined, extendWithDataDefinition env name constructors)
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
evalIn env (PTypeSig ts) = (Undefined, typeSigToEnv env ts)
evalIn env (PCase ts cond cases) =
  let condVal = fst $ evalIn env cond
      findFun (pred, predDo) = matchingDefinition env condVal (FunctionVal ts env [pred] predDo)
   in case List.find findFun cases of
        Just (pred, predDo) -> evalIn (extend env pred cond) predDo
evalIn env (Lambda ts args e) =
  (FunctionVal (ts {typeSigModule = inModule env}) env args e, env)
evalIn env (HFI f args) = hfiFun env f args
evalIn env (App e1 e2) = apply env e1 e2
evalIn env (Atom _ts atomId) = case inScope env atomId of
  [definition] -> (definition, env)
  [] -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope" ++ show (map Map.keys (envScopes env))
  definitions -> case last definitions of
    FunctionVal {} -> (Pattern definitions, env)
    lastDef -> (lastDef, env)
evalIn env (PInterpolatedString parts) =
  let fn x = case fst $ evalIn env x of
        StringVal s -> s
        o -> prettyVal o
   in (StringVal $ List.intercalate "" $ map fn parts, env)
evalIn env (PString n) = (StringVal n, env)
evalIn env (PFloat n) = (FloatVal n, env)
evalIn env (PInteger n) = (IntVal n, env)
evalIn env (PDictUpdate baseDict updateDict) =
  let (DictVal d1) = fst $ evalIn env baseDict
      (DictVal d2) = fst $ evalIn env updateDict
   in (DictVal $ Map.union d2 d1, env)
evalIn env (DictAccess k dict) =
  let (DictVal m) = fst $ evalIn env dict
      kv = fst $ evalIn env k
   in (fromJust (Map.lookup kv m), env)
evalIn env (PDictKey k) = (DictKey k, env)
evalIn env (PDict _ts pairs) =
  let fn (k, v) =
        let key = case fst $ evalIn env k of
              (DictKey s) -> DictKey s
              (StringVal s) -> DictKey s
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
evalIn env (Binop Assign (PTuple _ts1 bindings) (PTuple _ts2 vs)) =
  let evaledValues = map (fst . evalIn env) vs
      newEnv = extendWithTuple env bindings evaledValues
   in if length bindings /= length evaledValues
        then throw $ EvalException "Destructuring failed. Mismatched parameter count"
        else (TupleVal evaledValues, newEnv)
evalIn env (Binop Concat e1 e2) = evalIn env (App (App (lambda 2 "concat") e1) e2)
evalIn env (Binop Pipe e1 e2) = evalIn env (App e2 e1)
evalIn env (Binop FmapPipe e1 e2) = evalIn env (App (App (Atom anyTypeSig "fmap") e2) e1)
evalIn env (Binop Cons e1 e2) = evalIn env (App (App (lambda 2 "cons") e1) e2)
evalIn env (Binop Assign atom@(Atom _ts funName) (Lambda ts args e)) =
  let ts' = fromMaybe ts $ typeFromEnv env funName
      value = fst $ evalIn env (Lambda ts' args e)
      env' = extend env atom value
   in if length args /= length (typeSigIn ts')
        then error $ "Function definition argument count differs from typesig argument count for function " ++ show funName
        else (value, env')
evalIn env (Binop Assign expr v) =
  let env' = extend env expr v
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

lambda :: Int -> Id -> Expr
lambda argCount name = Lambda anyTypeSig args $ HFI name $ PList anyTypeSig args
  where
    args = take argCount $ map (Atom anyTypeSig . (: [])) ['a' ..]

extendWithDataDefinition :: Env -> String -> [ConstructorWithArgs] -> Env
extendWithDataDefinition env typeCons = foldl' foldFun env
  where
    foldFun :: Env -> ConstructorWithArgs -> Env
    foldFun accEnv (vc, args) = extend accEnv (Atom anyTypeSig vc) (DataConstructorDefinitionVal typeCons args)

extendWithTuple :: Env -> [Expr] -> [Val] -> Env
extendWithTuple env bindings vs =
  let foldFun accEnv (binding, val) = extend accEnv binding val
   in foldl' foldFun env (zip bindings vs)

extendWithTrait :: Env -> String -> [Expr] -> Env
extendWithTrait env name defs = extend env (Atom anyTypeSig name) $ TraitVal name defs

extendWithImplementation :: Env -> Trait -> [Expr] -> Env
extendWithImplementation env trait = foldl' foldFun env
  where
    foldFun accEnv def = case def of
      (Binop Assign atom@(Atom _ts funName) (Lambda ts args e)) ->
        let newTS = updateTS ts funName
         in extend accEnv atom (Lambda newTS args e)
      (Binop Assign atom val) -> extend accEnv atom val
    updateTS oldTS funName =
      (tsFromTraitDef funName) {typeSigTraitBinding = typeSigTraitBinding oldTS, typeSigImplementationBinding = typeSigImplementationBinding oldTS}
    tsFromTraitDef funName = case inScope env trait of
      [TraitVal _ defs] -> case List.find (findDef funName) defs of
        Just (PTypeSig ts) -> ts
        _ -> anyTypeSig
      _ -> error $ "Got more than one trait definition for " ++ show trait
    findDef funName (PTypeSig ts) = typeSigName ts == Just funName

-- let {a: b} = {a: 1} in
-- let {a: 1, b: c} = {a: 1, b: 2} in
-- let {a: b, ...} = {a: 1, b: 2, c: 2} in
extendWithDict :: Env -> [(Expr, Expr)] -> Map.Map Val Val -> Env
extendWithDict env exprPairs valMap = foldl' foldFun env exprPairs
  where
    foldFun :: Env -> (Expr, Expr) -> Env
    foldFun accEnv expr = case expr of
      (PDictKey a, atom@(Atom _ts atomId)) -> case Map.lookup (DictKey a) valMap of
        Just val -> extend accEnv atom val
        Nothing -> error "Dictionary doesn't contain key"
      (PDictKey a, expr) ->
        let evaledExpr = fst $ evalIn accEnv expr
         in ( case Map.lookup (DictKey a) valMap of
                Just val -> if val == evaledExpr then accEnv else error "Dictionary not matching pattern"
                Nothing -> error "Dictionary doesn't contain key"
            )

extendWithConsList :: Env -> [String] -> [Val] -> Env
extendWithConsList env bindings vs
  | length bindings - 1 > length vs = error "Too many bindings"
  | otherwise =
    let llast = (last bindings, ListVal $ drop (length bindings - 1) vs)
        linit = zip (take (length bindings - 1) bindings) vs
     in foldFun llast (foldr foldFun env linit)
  where
    foldFun (binding, val) accEnv = extend accEnv (Atom anyTypeSig binding) val

extendWithDataConstructor :: Env -> [Expr] -> [Val] -> Env
extendWithDataConstructor env exprs vals = foldl' foldFun env (zip exprs vals)
  where
    foldFun :: Env -> (Expr, Val) -> Env
    foldFun accEnv (expr, val) = extend accEnv expr val

extend :: Evaluatable e => Env -> Expr -> e -> Env
extend env argExpr expr = case argExpr of
  (Atom _ id) -> extendAtom env id expr
  (PDataConstructor _consName exprs) -> case val of
    (DataVal _dtype _name vals) -> extendWithDataConstructor env exprs vals
    s -> error $ "Non-value-cons value received for value cons destructuring" ++ show s
  (PDict _ts exprPairs) -> case val of
    (DictVal valMap) -> extendWithDict env exprPairs valMap
    _ -> error "Non-dict value received for dict destructuring"
  (ConsList bindings) -> case val of
    (ListVal vals) -> extendWithConsList env bindings vals
    _ -> error "Non-list value received for list destructuring"
  (PTuple _ts bindings) -> case val of
    (TupleVal vals) -> extendWithTuple env bindings vals
    s -> error $ "Non-tuple value " ++ show s ++ " received for tuple destructuring"
  _ -> env
  where
    val = fst $ evalIn env $ toExpr expr

extendAtom :: Evaluatable e => Env -> String -> e -> Env
extendAtom env "_" _ = env
extendAtom env id ex = env {envScopes = newScopes}
  where
    newScopes = init (envScopes env) ++ [Map.insertWith insertFun id [envEntry] (last (envScopes env))]
    insertFun = if id == "@" then const else flip (++)
    envEntry =
      EnvEntry
        { envEntryValue = fst $ evalIn env $ toExpr ex,
          envEntryModule = inModule env
        }

hfiFun :: Evaluatable e => Env -> Id -> e -> (Val, Env)
hfiFun env f argsList = case evaledArgsList of
  ListVal evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    evaledArgsList = fst $ evalIn env $ toExpr argsList
    fun "cons" (a : b : _) = case (a, b) of
      (a', ListVal bs) -> ListVal $ a' : bs
      (StringVal a', StringVal bs) -> StringVal $ a' ++ bs -- NOTE: this concats instead of conses
      s -> error $ show s
    fun "concat" (a : b : _) = case (a, b) of
      (ListVal a', ListVal bs) -> ListVal $ a' ++ bs
      (StringVal a', StringVal bs) -> StringVal $ a' ++ bs
      s -> error $ show s
    fun "zipWith" (fun : ListVal xs : ListVal ys : _) =
      let zipFun :: Evaluatable e => e -> e -> Val
          zipFun x y = fst $ evalIn env $ App (App (funToExpr fun) x) y
       in ListVal $ zipWith zipFun xs ys
    fun "toChars" ((StringVal s) : _) = ListVal $ map (StringVal . (: [])) s
    fun "dictToList" (dict : _) = case dict of
      (DictVal d) -> ListVal $ map (\(DictKey k, v) -> TupleVal [StringVal k, v]) (Map.toList d)
    fun "readFile" (StringVal path : _) = StringVal $ unsafePerformIO $ readFile' (envLangPath env ++ path)
    fun "writeFile" (StringVal path : StringVal body : _) = let !file = (unsafePerformIO $ writeFile path body) in StringVal body
    fun "sleep" (a : _) = unsafePerformIO (threadDelay 1000000 >> pure a)
    fun "getArgs" _ = ListVal $ map StringVal $ unsafePerformIO getArgs
    fun "print" (a : _) = unsafePerformIO (putStrLn (prettyVal a) >> pure a)
    fun "decodeJSON" ((StringVal s) : _) = jsonToVal $ BS.pack s
    fun "encodeJSON" (a : _) = StringVal $ BS.unpack $ encode a
    fun "debug" (a : b : _) = unsafePerformIO (print a >> pure b)
    fun x r = error ("No such HFI " ++ show x ++ show r)
    funToExpr (FunctionVal ts _env args e) = Lambda ts args e
    funToExpr (Pattern defs) = PatternExpr defs
    funToExpr r = error $ show r

apply :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
apply env e1 e2 =
  trace ("apply again") $
    case evalIn env e1 of
      (Pattern definitions, env') -> trace ("calling f with x = " ++ show (length definitions)) $ case List.filter (matchingDefinition env' passedArg) definitions of
        [] -> error "Could not find matching function definition"
        [FunctionVal ts _ args e3] -> callFunction env' ts args e2 e3
        funs@((FunctionVal ts _ args e3) : _) ->
          if functionFullyApplied args
            then callFullyApplied env' ts args e2 e3
            else
              let (appliedFuns, accEnv) = foldl' toFunVal ([], env') funs
                  toFunVal (accFuns, accEnv) fun = case fun of
                    FunctionVal ts _ args e3 ->
                      let (val, env'') = runFun accEnv ts args e2 e3
                       in (accFuns ++ [val], env'')
                    _ -> error "Non-function application"
               in trace ("APFUNS: " ++ show appliedFuns) $ (Pattern appliedFuns, accEnv)
      (FunctionVal ts _ args e3, env') -> callFunction env' ts args e2 e3
      (val, env) -> error ("Cannot apply value" ++ show val ++ " in env: " ++ show env)
  where
    (passedArg, _) = trace ("expr: " ++ show e2) $ evalIn env $ toExpr e2
    functionFullyApplied args = length args == 1
    callFullyApplied env ts args e2 e3 =
      let (val, env'') = runFun (setScope env (typeSigModule ts)) ts args e2 e3
       in (val, unsetScope env'' (inModule env))
    callFunction env ts args e2 e3 =
      if functionFullyApplied args
        then callFullyApplied env ts args e2 e3
        else runFun env ts args e2 e3

runFun :: (Evaluatable e1, Evaluatable e2) => Env -> TypeSig -> ArgsList -> e1 -> e2 -> (Val, Env)
runFun env ts argsList expr funExpr =
  let (arg : remainingArgs') = argsList
      newEnv = extend env arg expr
   in if null remainingArgs'
        then evalIn newEnv $ toExpr funExpr
        else evalIn newEnv (Lambda ts remainingArgs' funExpr)

eval :: Expr -> Val
eval = fst . evalIn emptyEnv

evalsIn :: Env -> [Expr] -> (Val, Env)
evalsIn env = foldl' fl (Undefined, env)
  where
    fl (!_val, env) ex = evalIn (resetScope env) ex

evals :: [Expr] -> Val
evals exprs = fst $ evalsIn emptyEnv exprs
