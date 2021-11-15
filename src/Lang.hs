module Lang where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Typeable
import Debug.Trace
import Eval
import Exceptions
import Parser
import Syntax
import System.Console.Haskeline
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
    allExprs lib = map parseExpr lib ++ exprs
    fl (_val, env) ex = evalInEnv env ex

repl :: IO ()
repl = replWithEnv emptyEnv

replWithEnv :: Env -> IO ()
replWithEnv env = runInputT defaultSettings $ do
  input <- getInputLine "lang > "
  case input of
    Nothing -> outputStrLn "Noop"
    Just "quit" -> return ()
    Just finput -> do
      case parseExpr' finput of -- catches parsing errors, but not evaluation errors
        Left e -> do
          outputStrLn "\n-- PARSE ERROR\n"
          outputStrLn $ show e ++ "\n"
          liftIO $ replWithEnv env
        Right expr -> do
          result <- liftIO $ try $ evalsWithLibAndEnv env [expr] :: InputT IO (Either LangException (Val, Env))
          case result of
            Left e -> outputStrLn "\n-- EVAL ERROR\n"
            Right (val, newenv) -> do
              outputStrLn $ show val ++ " : " ++ show (typeOf val)
              liftIO $ replWithEnv newenv

stdLib :: IO [String]
stdLib = do
  content <- readFile "src/stdlib/stdlib.lang"
  pure $ filter (not . null) $ lines content
