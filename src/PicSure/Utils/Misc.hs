module PicSure.Utils.Misc where

import Control.Monad.Plus

fromJust Nothing  = error "could not apply fromJust to Nothing"
fromJust (Just a) = a

fromLeft  (Left e)  = e
fromRight (Right e) = e

rightToMaybe (Left e)  = Nothing
rightToMaybe (Right e) = Just e

boolToMaybe boolf e = if boolf e then Just e else Nothing

isJust (Just _) = True
isJust _ = False

(?) a b bool = if bool then a else b
infixr 8 ?

just_or_default e Nothing  = e
just_or_default _ (Just e) = e
  
(>>>) f g = (f >>= (\e -> g e >> return e))

applyN 0 f x = x
applyN n f x = applyN (n-1) f (f x)

applyToSnd f (a, b) = (a, f b)
applyToFst f (a, b) = (f a, b)

perhaps b f = if b then f else id

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
