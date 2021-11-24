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

type RawStdLib = String

haskelineSettings :: Settings IO
haskelineSettings =
  Settings
    { historyFile = Just ".haskeline_history",
      autoAddHistory = True
    }

evaledStdLibEnv :: IO Env
evaledStdLibEnv = snd . evalsIn emptyEnv . parseExprs <$> rawStdLib

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
          outputStrLn $ "*** " ++ show e
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

rawStdLib :: IO String
rawStdLib = readFile "src/stdlib/stdlib.lang"
