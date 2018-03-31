{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module PicSure.Utils.Json where

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import PicSure.Utils.Misc

decodeValue :: BSL.ByteString -> Maybe Value
decodeValue = decode

decodeValue' :: (Maybe BSL.ByteString) -> Value
decodeValue' = fromJust . (>>=decodeValue)


lookup :: T.Text -> Value -> Value
lookup k (Object o) = fromJust $ HM.lookup k o

unString :: Value -> String
unString (String s) = T.unpack s

unNumber (Number n) = n

unArray (Array a) = V.toList a

stringToValue = String . T.pack

prettyJson :: Value -> BSL.ByteString
prettyJson = Pretty.encodePretty

jsonPrettyPrint :: Maybe Value -> IO ()
jsonPrettyPrint (Just e) = BSL.putStrLn . Pretty.encodePretty $ e
jsonPrettyPrint Nothing = BSL.putStrLn "Nothing"

tryPrettyJson :: BSL.ByteString -> BSL.ByteString
tryPrettyJson s = (\case Nothing -> s
                         Just s' -> Pretty.encodePretty s') $ (decode s :: Maybe Value)
