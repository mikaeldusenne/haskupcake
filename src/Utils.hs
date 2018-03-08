module Utils where

import Data.List
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Data.Text as T
import General
import Trees


(</>) "" b = b
(</>) a "" = a
(</>) a b = let n = f isSuffixOf a + f isPrefixOf b
                f g e = if g "/" e then 1 else 0
            in case n of 0 -> a ++ '/' : b
                         1 -> a ++ b
                         2 -> a ++ tail b
                         

lookup :: T.Text -> Value -> Value
lookup k (Object o) = fromJust $ HM.lookup k o

unString :: Value -> String
unString (String s) = T.unpack s

