module PicSure.Utils.General where

fromJust Nothing = error "could not apply fromJust to Nothing"
fromJust (Just a) = a

fromLeft (Left e) = e
fromRight (Right e) = e

isJust (Just _) = True
isJust _ = False

digit = "0123456789"
alphalower = "azertyuiopqsdfghjklmwxcvbn"
alphaUpper = "AZERTYUIOPQSDFGHJKLMWXCVBN"
alpha = alphalower ++ alphaUpper
alpha_num = digit ++ alpha
number = digit ++ "."
word = alpha_num ++ "_"

isDigit  = (`elem`digit)
isNumber = (`elem`number)
isLetter = (`elem`alpha)
isWord   = (`elem`word)
isSpace  = (`elem`" \t")

-- <if> 'function'
(!∫) a b bool = if bool then a else b
orelse = (!∫)

(!∫∫) a b f e = if f e then a e else b e

(?) a b bool = if bool then a else b
infixr 8 ?

just_or_default e Nothing  = e
just_or_default _ (Just e) = e
  
(>>>) f g = (f >>= (\e -> g e >> return e))

applyN 0 f x = x
applyN n f x = applyN (n-1) f (f x)

(!!?) :: [a] -> Int -> Maybe a
(!!?) l n = if n < 0 || n >= length l then Nothing else Just $ l !! n
