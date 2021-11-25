module Repl (repl) where

import Control.Exception
import Control.Monad.IO.Class
import Data.List qualified as List
import Data.List.Split
import Eval
import Lang
import Parser
import Syntax
import System.Console.Haskeline
import Text.Megaparsec.Error

haskelineSettings :: Settings IO
haskelineSettings =
  Settings
    { historyFile = Just ".haskeline_history",
      autoAddHistory = True
    }

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
          outputStrLn $ "*** " ++ (errorBundlePretty e)
          liftIO $ replWithEnv env
        Right exprs -> do
          env <- liftIO evaledStdLibEnv
          evaled <- liftIO . try $ evaluate $ evalsIn env exprs
          case evaled of
            Left (e :: SomeException) -> do
              outputStrLn $ "*** " ++ show e
              liftIO $ replWithEnv env
            Right (val, newEnv) -> do
              outputStrLn $ show val ++ " : " ++ show (toLangType val)
              liftIO $ replWithEnv newEnv
