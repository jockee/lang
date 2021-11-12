module Lang where

import Data.List qualified as List
import Data.Map qualified as Map
import Eval
import Parser
import Syntax

evalsWithLib :: [Expr] -> Val
evalsWithLib exprs = fst $ Prelude.foldl fl (Undefined, Map.empty) allExprs
  where
    allExprs = map parseExpr stdLib ++ exprs
    fl (_val, env) ex = evalInEnv env ex

stdLib :: [String]
stdLib = ["global = 0"]

-- stdLib = ["map = (f xs: fold (acc x: acc ++ [f x]) [])"]
