module Main where

import Control.Monad.Trans
import Eval
import Parser
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = undefined

-- main = getArgs >>= print . eval . readExpr . head
