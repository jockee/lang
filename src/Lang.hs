module Lang where

import Data.Maybe
import Debug.Trace
import Eval
import Parser
import System.Environment
import Types
import Util

e :: String -> IO Val
e = evalsInStdLib

ee :: String -> IO (Val, Env)
ee = evalsInStdLibWithEnv

evalsInStdLib :: String -> IO Val
evalsInStdLib rawExprs = do
  (val, _env) <- evalsInStdLibWithEnv rawExprs
  pure val

evalsInStdLibWithEnv :: String -> IO (Val, Env)
evalsInStdLibWithEnv rawExprs = do
  env <- evaledStdLibEnv
  pure $ evalsIn env $ parseExprs rawExprs

evaledStdLibEnv :: IO Env
evaledStdLibEnv = snd . evalsIn emptyEnv . parseExprs <$> rawStdLib

rawStdLib :: IO String
rawStdLib = do
  langPath <- lookupEnv "LANG_PATH"
  let path = maybe "" (\p -> if last p == '/' then p else p ++ "/") langPath
  stdLib <- readFile $ path ++ "src/stdlib/stdlib.lang"
  pure . strip $ stdLib
