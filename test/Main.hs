module Main where

import Spec qualified
import Test.Hspec.Formatters
import Test.Hspec.Runner

main :: IO ()
main = test

test = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
