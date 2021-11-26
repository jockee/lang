module Lang where

import Eval
import Parser
import Syntax

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
  types <- readFile "src/stdlib/types.lang"
  stdLib <- readFile "src/stdlib/stdlib.lang"
  pure $ types ++ stdLib
