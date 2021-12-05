module Http where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.CaseInsensitive qualified as CI
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import System.IO.Unsafe
import Types

request :: String -> String -> [(String, String)] -> String -> IO Val
request method url headers reqBody = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  request <- parseRequest url
  response <- httpLbs request manager
  pure $ responseToDict response

responseToDict :: Response L8.ByteString -> Val
responseToDict response = DataVal "Result" "Ok" dict
  where
    status = responseStatus response
    dict =
      [ DictVal $
          Map.fromList
            [ (DictKey "code", IntVal . fromIntegral $ statusCode status),
              (DictKey "reason", StringVal . decodeLatin1 $ statusMessage status),
              (DictKey "headers", DictVal . Map.fromList . parseHeaders $ responseHeaders response),
              (DictKey "body", StringVal . T.pack . L8.unpack $ responseBody response)
            ]
      ]
    parseHeaders :: [(CI.CI BS.ByteString, BS.ByteString)] -> [(Val, Val)]
    parseHeaders headers' =
      map
        ( bimap
            (StringVal . decodeLatin1 . CI.original)
            (StringVal . decodeLatin1)
        )
        headers'
