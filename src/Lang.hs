module Lang where

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
import System.IO
import System.IO.Unsafe
import TypeCheck

type RawStdLib = String

evaledStdLibEnv :: IO Env
evaledStdLibEnv = snd . evalsIn emptyEnv . parseExprs <$> rawStdLib

rawStdLib :: IO String
rawStdLib = readFile "src/stdlib/stdlib.lang"
