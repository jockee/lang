module Lang where

import Control.Monad
import Data.List qualified as List
import Data.Map qualified as Map
import Eval
import Parser
import Syntax
import System.IO

evalsWithLib :: [Expr] -> (Val, Env)
evalsWithLib = evalsWithLibAndEnv Map.empty

evalsWithLibAndEnv :: Env -> [Expr] -> (Val, Env)
evalsWithLibAndEnv env exprs = Prelude.foldl fl (Undefined, env) allExprs
  where
    allExprs = map parseExpr stdLib ++ exprs
    fl (_val, env) ex = evalInEnv env ex

repl :: IO ()
repl = replWithEnv (Map.empty :: Env)

replWithEnv :: Env -> IO ()
replWithEnv env = forever $ do
  hSetBuffering stdin LineBuffering
  putStr "> "
  expr <- getLine
  let (val, newenv) = evalsWithLibAndEnv env [parseExpr expr]
  print val
  replWithEnv newenv

stdLib :: [String]
stdLib = ["global = 0"]

-- stdLib = ["map = (f xs: fold (acc x: acc ++ [f x]) [])"]
-- TODO: Read from file(s)
