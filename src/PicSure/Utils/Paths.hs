module PicSure.Utils.Paths where

import PicSure.Utils.List
import Data.List

(</>) "" b = b
(</>) a "" = a
(</>) a b = let n = f isSuffixOf a + f isPrefixOf b
                f g e = if g "/" e then 1 else 0
            in case n of 0 -> a ++ '/' : b
                         1 -> a ++ b
                         2 -> a ++ tail b


splitPath = splitWhen "/"
