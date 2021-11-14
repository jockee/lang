module Lang where

import Control.Monad
import Control.Monad.IO.Class
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Typeable
import Debug.Trace
import Eval
import Parser
import Syntax
import System.IO

evalWithLib :: Expr -> IO Val
evalWithLib expr = do
  (val, _) <- evalsWithLibAndEnv emptyEnv [expr]
  pure val

-- NOTE: entry point for reading in non-stdlib source files?
evalsWithLib :: [Expr] -> IO (Val, Env)
evalsWithLib = evalsWithLibAndEnv emptyEnv

evalsWithLibAndEnv :: Env -> [Expr] -> IO (Val, Env)
evalsWithLibAndEnv env exprs = stdLib >>= (pure . foldl fl (Undefined, env) . allExprs)
  where
    allExprs lib = trace ("calling f with x = " ++ show lib) $ map parseExpr lib ++ exprs
    fl (_val, env) ex = evalInEnv env ex

repl :: IO ()
repl = replWithEnv emptyEnv

replWithEnv :: Env -> IO ()
replWithEnv env = forever $ do
  hSetBuffering stdin LineBuffering
  putStr "> "
  expr <- getLine
  (val, newenv) <- evalsWithLibAndEnv env [parseExpr expr]
  putStrLn $ show val ++ " : " ++ show (typeOf val)
  replWithEnv newenv

stdLib :: IO [String]
stdLib = do
  content <- readFile "src/stdlib/stdlib.lang"
  pure $ filter (not . null) $ lines content
