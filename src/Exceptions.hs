module Exceptions where

import Control.Exception
import Data.Typeable
import Text.Parsec.Error

data LangException
  = ParseException ParseError
  | EvalException String
  deriving (Show, Eq, Typeable)

instance Exception LangException where
  toException e = SomeException e
  fromException (SomeException e) = cast e -- uses Typeable
