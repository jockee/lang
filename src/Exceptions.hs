module Exceptions where

import Control.Exception
import Text.Parsec.Error

data LangException
  = ParseException ParseError
  | EvalException String
  deriving (Show, Eq)

instance Exception LangException