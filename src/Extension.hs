{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Extension where

import Control.Exception
import Data.List (foldl')
import Data.Map qualified as Map
import Debug.Trace
import Exceptions
import Types

extend' :: HasBindings h => Env -> h -> Expr -> Val -> h
extend' env bindable argExpr val = case argExpr of
  (Atom _ id) -> extendAtom env bindable id val
  (PDataConstructor _consName exprs) -> case val of
    (DataVal _dtype _name vals) -> extendWithDataConstructor env bindable exprs vals
    s -> error $ "Non-value-cons value received for value cons destructuring" ++ show s
  (PDict _ts exprPairs) -> case val of
    (DictVal valMap) -> extendWithDict env bindable exprPairs valMap
    _ -> error "Non-dict value received for dict destructuring"
  (Cons bindings) -> case val of
    (ListVal vals) -> extendWithCons env bindable bindings vals
    _ -> error "Non-list value received for cons destructuring"
  (PTuple _ts bindings) -> case val of
    (TupleVal vals) -> extendWithListable env bindable bindings vals
    s -> error $ "Non-tuple value " ++ show s ++ " received for tuple destructuring"
  (PList _ts bindings) -> case val of
    (ListVal vals) -> extendWithListable env bindable bindings vals
    _ -> error "Non-list value received for list destructuring"
  _ -> bindable

extendWithDataDefinition :: HasBindings h => Env -> h -> String -> [ConstructorWithArgs] -> h
extendWithDataDefinition env bindable typeCons = foldl' foldFun bindable
  where
    foldFun accBindable (vc, args) = extend' env accBindable (Atom anyTypeSig vc) (DataConstructorDefinitionVal typeCons args)

extendWithListable :: HasBindings h => Env -> h -> [Expr] -> [Val] -> h
extendWithListable env bindable bindings vs
  | length bindings /= length vs = throw $ EvalException "Destructuring failed. Mismatched parameter count"
  | otherwise = foldl' foldFun bindable (zip bindings vs)
  where
    foldFun accBindable (binding, val) = extend' env accBindable binding val

extendWithTrait :: HasBindings h => Env -> h -> String -> [Expr] -> h
extendWithTrait env bindable name defs = extend' env bindable (Atom anyTypeSig name) $ TraitVal name defs

extendWithDict :: HasBindings h => Env -> h -> [(Expr, Expr)] -> Map.Map Val Val -> h
extendWithDict env bindable exprPairs valMap = foldl' foldFun bindable exprPairs
  where
    foldFun accBindable expr = case expr of
      (PDictKey a, ex) -> case (ex, Map.lookup (DictKey a) valMap) of
        (_, Just val) -> extend' env accBindable ex val
        (_, Nothing) -> error "Dictionary doesn't contain key"

extendWithCons :: HasBindings h => Env -> h -> [Expr] -> [Val] -> h
extendWithCons env bindable bindings vs
  | length bindings - 1 > length vs = error $ "Too many bindings. Wanted " ++ show (length bindings) ++ ", but got " ++ show (length vs)
  | otherwise =
    let llast = (last bindings, ListVal $ drop (length bindings - 1) vs)
        linit = zip (take (length bindings - 1) bindings) vs
     in foldFun llast (foldr foldFun bindable linit)
  where
    foldFun (binding, val) accBindable = extend' env accBindable binding val

extendWithDataConstructor :: HasBindings h => Env -> h -> [Expr] -> [Val] -> h
extendWithDataConstructor env bindable exprs vals = foldl' foldFun bindable (zip exprs vals)
  where
    foldFun accEnv (expr, val) = extend' env accEnv expr val

extendAtom :: HasBindings h => Env -> h -> String -> Val -> h
extendAtom _ bindable "_" _ = bindable
extendAtom env bindable id val =
  setBindings bindable withUpdatedScope
  where
    withUpdatedScope = Map.insertWith insertFun id [envEntry] (getBindings bindable)
    insertFun = if id == "@" then const else flip (++)
    envEntry =
      EnvEntry
        { envEntryValue = val,
          envEntryModule = inModule env
        }
