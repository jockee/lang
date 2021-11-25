module Lang where

import Eval
import Parser
import Syntax

evalsInStdLib :: String -> IO Val
evalsInStdLib rawExprs = do
  env <- evaledStdLibEnv
  pure $ fst $ evalsIn env $ parseExprs rawExprs

evaledStdLibEnv :: IO Env
evaledStdLibEnv = snd . evalsIn emptyEnv . parseExprs <$> rawStdLib

rawStdLib :: IO String
rawStdLib = readFile "src/stdlib/stdlib.lang"
