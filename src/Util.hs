module Util where

import Data.Text qualified as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack
