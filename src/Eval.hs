{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Eval where

import Control.Concurrent
import Control.Exception
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace
import Exceptions
import Syntax
import System.IO.Unsafe
import TypeCheck

instance Evaluatable Val where
  evalIn env val = (val, env)
  toExpr val = undefined

instance Evaluatable Expr where
  toExpr expr = expr
  evalIn env (PTrait name defs) = (Undefined, extendWithTrait env name defs)
  evalIn env (PImplementation _trait for defs) = (Undefined, extendWithImplementation env for defs)
  evalIn env (Module name e) = evalsIn (moduleToEnv env name) e
  evalIn env (PDataDefinition name constructors) = (Undefined, extendWithDataDefinition env name constructors)
  evalIn env (PDataConstructor name exprArgs) =
    let evaledArgs = map (fst . evalIn env) exprArgs
     in case inScope env name of
          Just [DataConstructorDefinitionVal dtype argVals] ->
            if length exprArgs > length argVals
              then error $ "Too many arguments to value constructor " ++ show name
              else case List.find (\(_i, ex, val) -> not $ typeMatches env (toLangType ex) (toLangType val)) (zip3 [1 ..] exprArgs argVals) of
                Just (i, ex, val) -> error $ "Mismatched types. Wanted " ++ show (toLangType ex) ++ ", got: " ++ show (toLangType val) ++ " in position " ++ show (i + 1)
                Nothing -> (DataVal dtype name evaledArgs, env)
          Nothing -> error $ "No such constructor found: " ++ show name
  evalIn env (PTypeSig ts) = (Undefined, typeSigToEnv env ts)
  evalIn env (PCase ts cond cases) =
    let condVal = fst $ evalIn env cond
        findFun (pred, predDo) = patternMatch env condVal (FunctionVal ts env [pred] predDo)
     in case List.find findFun cases of
          Just (_pred, predDo) -> (fst $ evalIn env predDo, env)
  evalIn env (PIf (PBool True) t _) = evalIn env t
  evalIn env (PIf (PBool False) _ f) = evalIn env f
  evalIn env (PIf condition ifTrue ifFalse) =
    let (val, env') = evalIn env condition
     in if val == BoolVal True then evalIn env' ifTrue else evalIn env' ifFalse
  evalIn env (Lambda ts args e) =
    (FunctionVal ts env args e, env)
  evalIn env (InternalFunction f args) = internalFunction env f args
  evalIn env (App e1 e2) = apply (withScope env) e1 e2
  evalIn env (Atom _ts atomId) = case inScope env atomId of
    Just [definition] -> (definition, env)
    Just definitions -> case last definitions of
      FunctionVal {} -> (Pattern definitions, env)
      lastDef -> (lastDef, env)
    Nothing -> throw . EvalException $ "Atom " ++ atomId ++ " does not exist in scope"
  evalIn env (PString parts) =
    let fn x = case fst $ evalIn env x of
          StringVal s -> s
          o -> show o
     in (StringVal $ List.intercalate "" $ map fn parts, env)
  evalIn env (PChar n) = (StringVal n, env)
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
  evalIn env (Binop Concat e1 e2) =
    let (ListVal xs, _) = evalIn env e1
        (ListVal ys, _) = evalIn env e2
        _ = error "Invalid"
     in (ListVal $ xs ++ ys, env)
  evalIn env (Binop Pipe e1 e2) = apply (withScope env) e2 e1
  evalIn env (Binop Cons e1 e2) =
    let (v1, _) = evalIn env e1
     in case fst $ evalIn env e2 of
          ListVal xs -> (ListVal (v1 : xs), env)
  evalIn env (Binop Assign (Atom _ts a) v) =
    let env'' = extend env' a AnyType v
        (value, env') = evalIn env v
     in (value, env'')
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

extendWithDataDefinition :: Env -> String -> [ConstructorWithArgs] -> Env
extendWithDataDefinition env typeCons = foldl foldFun env
  where
    foldFun :: Env -> ConstructorWithArgs -> Env
    foldFun accEnv (vc, args) = extend accEnv vc AnyType (DataConstructorDefinitionVal typeCons args)

extendWithTuple :: Env -> [Expr] -> [Val] -> Env
extendWithTuple env bindings vs =
  let foldFun accEnv (binding, val) = case binding of
        Atom _ts atomId -> extend accEnv atomId AnyType val
        argExpr -> extendNonAtom accEnv argExpr val
   in foldl foldFun env (zip bindings vs)

extendWithTrait :: Env -> String -> [Expr] -> Env
extendWithTrait env name defs = extend env name AnyType $ TraitVal name defs

extendWithImplementation :: Env -> TypeConstructor -> [Expr] -> Env
extendWithImplementation env for = foldl foldFun env
  where
    foldFun accEnv def = case def of
      (Binop Assign (Atom _ts a) v) -> extend accEnv a AnyType v

-- let {a: b} = {a: 1} in
-- let {a: 1, b: c} = {a: 1, b: 2} in
-- let {a: b, ...} = {a: 1, b: 2, c: 2} in
extendWithDict :: Env -> [(Expr, Expr)] -> Map.Map Val Val -> Env
extendWithDict env exprPairs valMap = foldl foldFun env exprPairs
  where
    foldFun :: Env -> (Expr, Expr) -> Env
    foldFun accEnv expr = case expr of
      (PDictKey a, Atom _ts atomId) -> case Map.lookup (DictKey a) valMap of
        Just val -> extend accEnv atomId AnyType val
        Nothing -> error "Dict doesn't contain key"
      (PDictKey a, expr) ->
        let evaledExpr = fst $ evalIn accEnv expr
         in ( case Map.lookup (DictKey a) valMap of
                Just val -> if val == evaledExpr then accEnv else error "Dict not matching pattern"
                Nothing -> error "Dict doesn't contain key"
            )

extendWithConsList :: Env -> [String] -> [Val] -> Env
extendWithConsList env bindings vs
  | length bindings - 1 > length vs = error "Too many bindings"
  | otherwise =
    let llast = (last bindings, ListVal $ drop (length bindings - 1) vs)
        linit = zip (take (length bindings - 1) bindings) vs
     in foldFun llast (foldr foldFun env linit)
  where
    foldFun (binding, val) accEnv = extend accEnv binding AnyType val

extendWithDataConstructor :: Env -> String -> [Expr] -> [Val] -> Env
extendWithDataConstructor env name exprs vals = foldl foldFun env (zip exprs vals)
  where
    foldFun :: Env -> (Expr, Val) -> Env
    foldFun accEnv (Atom _ts atomId, val) = extend accEnv atomId AnyType val

extendNonAtom :: Evaluatable e => Env -> Expr -> e -> Env
extendNonAtom env argExpr expr = case argExpr of
  (PDataConstructor consName exprs) -> case val of
    (DataVal dtype name vals) -> extendWithDataConstructor env name exprs vals
    s -> error $ "Non-value-cons value received for value cons destructuring" ++ show s
  (PDict _ts exprPairs) -> case val of
    (DictVal valMap) -> extendWithDict env exprPairs valMap
    _ -> error "Non-dict value received for dict destructuring"
  (ConsList bindings) -> case val of
    (ListVal vals) -> extendWithConsList env bindings vals
    _ -> error "Non-list value received for list destructuring"
  (PTuple _ts bindings) -> case val of
    (TupleVal vals) -> extendWithTuple env bindings vals
    _ -> error "Non-tuple value received for tuple destructuring"
  _ -> env
  where
    val = fst $ evalIn env expr

extend :: Evaluatable e => Env -> Id -> LangType -> e -> Env
extend env "_" _ _ = env
extend env id expectedType ex =
  if expectedType == AnyType || gotType == expectedType
    then env {envValues = Map.insertWith (flip (++)) key [val] (envValues env)}
    else throw . RuntimeException $ "Expected type " ++ show expectedType ++ ", got " ++ show gotType
  where
    gotType = toLangType val
    newEnv = env {envValues = Map.insertWith (flip (++)) key [val] (envValues env)}
    val = fst $ evalIn env ex
    key = last (envScopes env) ++ ":" ++ id

internalFunction :: Evaluatable e => Env -> Id -> e -> (Val, Env)
internalFunction env f argsList = case evaledArgsList of
  ListVal evaledArgs -> (fun f evaledArgs, env)
  _ -> error "Got non-list"
  where
    evaledArgsList = fst $ evalIn env argsList
    fun "fold" (fun : init : ListVal xs : _) =
      let foldFun acc x = fst $ evalIn env $ App (App (funToExpr fun) acc) x
       in foldl foldFun init xs
    fun "zipWith" (fun : ListVal xs : ListVal ys : _) =
      let zipFun :: Evaluatable e => e -> e -> Val
          zipFun x y = fst $ evalIn env $ App (App (funToExpr fun) x) y
       in ListVal $ zipWith zipFun xs ys
    fun "head" xs = case xs of
      [] -> DataVal "Maybe" "None" []
      (x : _) -> DataVal "Maybe" "Some" [x]
    fun "dictToList" (dict : _) = case dict of
      (DictVal d) -> ListVal $ map (\(k, v) -> TupleVal [k, v]) (Map.toList d)
    fun "sleep" (a : _) = unsafePerformIO (threadDelay 1000000 >> pure a)
    fun "print" (a : _) = unsafePerformIO (print a >> pure a)
    fun "debug" (a : b : _) = unsafePerformIO (print a >> pure b)
    fun "sort" xs = ListVal . List.sort $ xs
    fun x r = error ("No such function " ++ show x ++ show r)
    funToExpr (FunctionVal ts _env args e) = Lambda ts args e

apply :: Evaluatable e => Env -> Expr -> e -> (Val, Env)
apply env e1 e2 =
  case evalIn env e1 of
    (Pattern definitions, env') -> case List.filter (patternMatch env' passedArg) definitions of
      [] -> error "Pattern match fail"
      [FunctionVal ts _ args e3] -> runFun env' ts args e2 e3
      (f : fs) -> case f of
        (FunctionVal ts _ args e3) ->
          if functionFullyApplied args
            then runFun env' ts args e2 e3
            else
              let (appliedFuns, accEnv) = foldl toFunVal ([], env') (f : fs)
                  toFunVal :: ([Val], Env) -> Val -> ([Val], Env)
                  toFunVal (accFuns, accEnv) fun =
                    case fun of
                      (FunctionVal ts _ args e3) ->
                        let (val, env'') = runFun accEnv ts args e2 e3
                         in (accFuns ++ [val], env'')
                      _ -> error "Non-function application"
               in (Pattern appliedFuns, accEnv)
    (FunctionVal ts _ args e3, env') -> runFun env' ts args e2 e3
    (val, env) -> error ("Cannot apply value" ++ show val ++ " in env: " ++ show env)
  where
    (passedArg, _) = evalIn env e2
    functionFullyApplied args = length args == 1

patternMatch :: Env -> Val -> Val -> Bool
patternMatch env passedArg definition = case definition of
  FunctionVal ts _ (expectedArgExp : _) _ ->
    let passedArgType = toLangType passedArg
        expectedType = toLangType expectedArgExp
     in typeMatches env expectedType passedArgType
          && traitBindingMatches passedArg ts
          && patternMatches expectedArgExp passedArg

traitBindingMatches :: Val -> TypeSig -> Bool
traitBindingMatches (DataVal dConsFromPassed _ _) ts = case typeSigTraitBinding ts of
  Nothing -> True
  dConsFromDef -> Just dConsFromPassed == dConsFromDef
traitBindingMatches _ _ = True

patternMatches :: Expr -> Val -> Bool
patternMatches (PList _ []) (ListVal []) = True
patternMatches (PList _ _) _ = False
patternMatches (PDataConstructor exprName _) (DataVal _ valName _) = exprName == valName
patternMatches (PInteger e) (IntVal v) = e == v
patternMatches (PFloat e) (FloatVal v) = e == v
patternMatches _ _ = True

runFun :: (Evaluatable e1, Evaluatable e2) => Env -> TypeSig -> ArgsList -> e1 -> e2 -> (Val, Env)
runFun env ts argsList expr funExpr =
  let allArgs@(arg : remainingArgs') = argsList
      newEnv =
        case arg of
          (Atom _ts atomId) -> extend env atomId (expectedType env ts $ length allArgs) expr
          argExpr -> extendNonAtom env argExpr expr
   in if null remainingArgs'
        then evalIn newEnv funExpr
        else evalIn newEnv (Lambda ts remainingArgs' funExpr)

eval :: Evaluatable e => e -> Val
eval = fst . evalIn emptyEnv

evalsIn :: Evaluatable e => Env -> [e] -> (Val, Env)
evalsIn env = foldl fl (Undefined, env)
  where
    fl (_val, env) ex = evalIn (resetScope env) ex

evals :: Evaluatable e => [e] -> Val
evals exprs = fst $ evalsIn emptyEnv exprs
