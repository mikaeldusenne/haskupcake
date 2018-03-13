module PicSure.Utils.Json where

import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import PicSure.Utils.Misc


lookup :: T.Text -> Value -> Value
lookup k (Object o) = fromJust $ HM.lookup k o

unString :: Value -> String
unString (String s) = T.unpack s

unNumber (Number n) = n

unArray (Array a) = V.toList a

stringToValue = String . T.pack
