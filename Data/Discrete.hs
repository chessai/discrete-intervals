{-# LANGUAGE MagicHash #-}

module Data.Discrete
  ( Discrete(..)
  ) where

import GHC.Base
import GHC.Char

class Discrete a where
  {-# MINIMAL pr, su #-}
  pr :: a -> a
  su :: a -> a

instance Discrete Bool where
  su False = True
  su True  = error "dont"

  pr True  = False
  pr False = error "dont"

instance Discrete Ordering where
  su LT = EQ
  su EQ = GT
  su GT = error "dont"

  pr GT = EQ
  pr EQ = LT
  pr LT = error "dont"

instance Discrete Int where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Char where
  su (C# c#)
    | isTrue# (ord# c# /=# 0x10FFFF#) = C# (chr# (ord# c# +# 1#))
    | otherwise = error "DONT"
  pr (C# c#)
    | isTrue# (ord# c# /=# 0#) = C# (chr# (ord# c# -# 1#))
    | otherwise = error "dont"

instance Discrete Integer where
  su x = x + 1
  pr x = x - 1

instance Discrete Word where
  su x
    | x /= maxBound = x + 1
    | otherwise     = error "dont"
  pr x
    | x /= minBound = x - 1
    | otherwise     = error "dont"

