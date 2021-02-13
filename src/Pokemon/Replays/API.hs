{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Replays.API where

import Control.Exception
import Data.ByteString.Lazy as B
import Data.Functor
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Pokemon.Replays.Parsers
import Text.Parsec

settings = tlsManagerSettings

getReplay :: String -> IO (Maybe Text )
getReplay path = do
  manager <- newManager settings
  response <- catch (httpLbs (parseRequest_ (path ++ ".log")) manager <&> Just) (\(HttpExceptionRequest req content) -> return Nothing)
  if isJust response
    then do
      return $ decodeLatin1 . B.toStrict . responseBody <$> response
    else do
      return Nothing