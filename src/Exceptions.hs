module Exceptions where

import Control.Exception
import Data.Typeable
import Text.Parsec.Error

data LangException
  = ParseException ParseError
  | EvalException String
  | RuntimeException String
  | TypeException String
  deriving stock (Show, Eq)

instance Exception LangException where
  toException e = SomeException e
  fromException (SomeException e) = cast e
