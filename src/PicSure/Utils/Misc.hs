module PicSure.Utils.Misc where

import Control.Monad.IO.Class

-- import Control.Monad.Plus

alphaNum = "azertyuiopmlkjhgfdsqwxcvbnAZERTYUIOPMLKJHGFDSQWXCVBN1234567890"

fromJust Nothing  = error "could not apply fromJust to Nothing"
fromJust (Just a) = a

fromLeft  (Left e)  = e
fromLeft  (Right _) = error "fromLeft error"

fromRight (Right e) = e
fromRight (Left _) = error "fromRight error"

rightToMaybe (Left _)  = Nothing
rightToMaybe (Right e) = Just e

boolToMaybe boolf e = if boolf e then Just e else Nothing

isJust (Just _) = True
isJust _ = False

(?) a b bool = if bool then a else b
infixr 8 ?


just_or_default e Nothing  = e
just_or_default _ (Just e) = e
  
(>>>) f g = (f >>= (\e -> g e >> return e))

applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

applyToSnd f (a, b) = (a, f b)
applyToFst f (a, b) = (f a, b)

perhaps b f = if b then f else id
perhaps' bf f e = perhaps (bf e) f $ e

perhapsM fb f e = fb e >>= \b -> if b then f e else return e

  
print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn
