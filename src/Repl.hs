{-# LANGUAGE BangPatterns #-}

module Repl (repl) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Text qualified as T
import Debug.Trace
import Eval
import Lang
import Parser
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple
import TypeCheck
import Types

haskelineSettings :: Settings IO
haskelineSettings =
  Settings
    { historyFile = Just ".haskeline_history",
      autoAddHistory = True
    }

repl :: IO ()
repl = evaledStdLibEnv >>= replWithEnv

replWithEnv :: Env -> IO ()
replWithEnv env = runInputT haskelineSettings $ loop env
  where
    loop :: Env -> InputT IO ()
    loop !env = do
      input <- getInputLine "lang > "
      case input of
        Nothing -> outputStrLn "Noop"
        Just "" -> loop env
        Just "quit" -> return ()
        Just "env" -> do
          pPrint env
          loop env
        Just finput -> do
          case parseExprs' "REPL" finput of
            Left e -> do
              pPrint $ errorBundlePretty e
              loop env
            Right exprs -> do
              evaled <- liftIO . try $ evaluate $ evalsIn env exprs
              case evaled of
                Left (e :: SomeException) -> do
                  pPrint e
                  loop env
                Right (val, newEnv) -> do
                  outputStrLn $ prettyVal val ++ " : " ++ prettyLangType (toLangType val)
                  loop (extend (resetScope newEnv) (Atom anyTypeSig "@") val)
