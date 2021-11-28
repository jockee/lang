{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Text qualified as T
import Debug.Trace
import Eval
import Exceptions
import Lang
import Parser
import Repl
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple
import Text.Printf
import Util

main = do
  (args, filePath) <- getArgs >>= parse
  handleInput args filePath

handleInput [Eval str] filePath = exec "eval" str
handleInput args filePath = do
  contents <- readFile filePath
  exec filePath contents

exec :: String -> String -> IO ()
exec source str = case parseExprs' source (strip str) of
  Left e -> do
    pPrint $ errorBundlePretty e
  Right exprs -> do
    env <- evaledStdLibEnv
    evaled <- try $ evaluate $ evalsIn env exprs
    case evaled of
      Left (e :: SomeException) -> do
        pPrint e
      Right (!val, newEnv) -> return ()

data Flag
  = Eval String -- -e
  | Help -- --help
  | REPL -- --repl
  deriving (Eq, Ord, Show)

options =
  [ Option ['e'] ["eval"] (ReqArg Eval "STRING") "Eval STRING",
    Option [] ["repl"] (NoArg REPL) "REPL",
    Option [] ["help"] (NoArg Help) "Print this help message"
  ]

parse argv = case getOpt Permute options argv of
  (args, fs, []) -> do
    let files = if null fs then ["-"] else fs
    if Help `elem` args
      then do
        hPutStrLn stderr (usageInfo header options)
        exitWith ExitSuccess
      else
        if REPL `elem` args
          then repl >> exitWith ExitSuccess
          else return (nub args, head files)
  (_, _, errs) -> do
    hPutStrLn stderr (concat errs ++ usageInfo header options)
    exitWith (ExitFailure 1)
  where
    header = "Usage: lang [-e] [file ...]"
