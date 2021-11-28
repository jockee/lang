module Main where

import Eval
import Lang
import Parser
import Repl
import Spec qualified
import Test.Hspec.Formatters
import Test.Hspec.Runner
import Types

main :: IO ()
main = test

test = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
