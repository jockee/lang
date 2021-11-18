module TypeCheckSpec where

import Control.Exception
import Data.Map qualified as Map
import Data.Typeable
import Debug.Trace
import Exceptions
import Lang
import Parser
import Syntax
import Test.Hspec
import TypeCheck

spec :: Spec
spec = describe "TypeCheck" $ do
  describe "TypeCheck" $ do
    xit "Checks return type (primitive)" $ do
      typeCheckMany (parseExprs "a :: Integer -> Integer; a b := 1.0") `shouldBe` (Left "OK")

    xit "Checks argument type (primitive)" $ do
      typeCheckMany (parseExprs "a :: Integer -> Integer; a b := 1; a 1.0") `shouldBe` Left "OK"

    it "Checks argument type (second argument)" $ do
      typeCheckMany (parseExprs "a :: Integer -> Integer -> Integer; a b := 1; a 1 1.0") `shouldBe` Left "OK"

    xit "Checks argument type (primitive through atom)" $ do
      typeCheckMany (parseExprs "a :: Integer -> Integer; a b := 1; c = 1.0; a c") `shouldBe` Left "OK"
