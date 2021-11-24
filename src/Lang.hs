module Lang where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Typeable
import Debug.Trace
import Eval
import Exceptions
import Parser
import Syntax
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.IO
import System.IO.Unsafe
import TypeCheck

haskelineSettings :: Settings IO
haskelineSettings =
  Settings
    { historyFile = Just ".haskeline_history",
      autoAddHistory = True
    }

evalWithLib :: Expr -> Val
evalWithLib expr = fst $ evalsWithLibAndEnv emptyEnv [expr]

-- NOTE: entry point for reading in non-stdlib source files?
evalsWithLib :: [Expr] -> (Val, Env)
evalsWithLib = evalsWithLibAndEnv emptyEnv

evalsWithLibAndEnv :: Env -> [Expr] -> (Val, Env)
evalsWithLibAndEnv env exprs = foldl fl (Undefined, env) allExprs
  where
    allExprs = parseExprs stdLib ++ exprs
    fl (_val, env) ex = evalIn (resetScope env) ex

repl :: IO ()
repl = replWithEnv emptyEnv

replWithEnv :: Env -> IO ()
replWithEnv env = runInputT haskelineSettings $ do
  input <- getInputLine "lang > "
  case input of
    Nothing -> outputStrLn "Noop"
    Just "quit" -> return ()
    Just finput -> do
      case parseExprs' finput of
        Left e -> do
          outputStrLn "\n# Parse error\n"
          outputStrLn $ show e ++ "\n"
          liftIO $ replWithEnv env
        Right exprs -> do
          s <- liftIO . try $ evaluate $ evalsWithLibAndEnv env exprs
          case s of
            Left (e :: SomeException) -> do
              outputStrLn "\n# Evaluation error\n"
              outputStrLn $ show e ++ "\n"
              liftIO $ replWithEnv env
            Right (val, newEnv) -> do
              outputStrLn $ show val ++ " : " ++ show (toLangType val)
              liftIO $ replWithEnv newEnv

stdLib :: String
stdLib = unsafePerformIO $ readFile "src/stdlib/stdlib.lang"
