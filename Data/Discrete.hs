{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.Discrete
  ( Discrete(..)
  ) where

import Data.Fixed
import Data.Monoid
import Data.Proxy
import Data.Functor.Const
import Data.Functor.Identity
import Data.Semigroup hiding (First(..), Last(..))
import Data.Type.Coercion
import Data.Type.Equality
import GHC.Base
import GHC.Char
import GHC.Enum (Bounded(..))
import GHC.Int
import GHC.Num
import GHC.Real
import GHC.Unicode
import GHC.Word

class Discrete a where
  {-# MINIMAL pr, su #-}
  pr :: a -> a
  su :: a -> a

deriving newtype instance Discrete (f a) => Discrete (Alt f a)
deriving newtype instance Discrete a => Discrete (Identity a)
deriving newtype instance Discrete a => Discrete (Const a b)
deriving anyclass instance Discrete (GeneralCategory)

instance Discrete (Fixed a) where
  su (MkFixed a) = MkFixed (su a)
  pr (MkFixed a) = MkFixed (pr a)

instance Discrete a => Discrete (Maybe a) where
  su Nothing  = error "dont"
  su (Just x) = Just (su x)
  pr Nothing  = error "dont"
  pr (Just x) = Just (pr x)

instance Integral a => Discrete (Ratio a) where
  {-# SPECIALIZE instance Discrete Rational #-} 
  su x = x + 1
  pr x = x - 1

instance Discrete a => Discrete (Min a) where
  su (Min a) = Min (su a)
  pr (Min a) = Min (pr a)

instance Discrete a => Discrete (Max a) where
  su (Max a) = Max (su a)
  pr (Max a) = Max (pr a)

instance Discrete a => Discrete (First a) where
  su (First a) = First (su a)
  pr (First a) = First (pr a)

instance Discrete a => Discrete (Last a) where
  su (Last a) = Last (su a)
  pr (Last a) = Last (pr a)

instance Discrete a => Discrete (WrappedMonoid a) where
  su (WrapMonoid a) = WrapMonoid (su a)
  pr (WrapMonoid a) = WrapMonoid (pr a)

instance a ~ b => Discrete (a :~: b) where
  pr _ = Refl
  su _ = Refl

instance a ~~ b => Discrete (a :~~: b) where
  pr _ = HRefl
  su _ = HRefl

instance Discrete (Proxy s) where
  su _ = error "dont"
  pr _ = error "dont"

instance Coercible a b => Discrete (Coercion a b) where
  su _ = error "dont"
  pr _ = error "dont"

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

instance Discrete Int where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Int8 where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Int16 where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Int32 where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Int64 where
  su x
    | x == maxBound = error "dont"
    | otherwise     = x + 1
  
  pr x
    | x == minBound = error "dont"
    | otherwise     = x - 1

instance Discrete Word where
  su x
    | x /= maxBound = x + 1
    | otherwise     = error "dont"
  pr x
    | x /= minBound = x - 1
    | otherwise     = error "dont"

instance Discrete Word8 where
    su x
        | x /= maxBound = x + 1
        | otherwise     = error "dont"
    pr x
        | x /= minBound = x - 1
        | otherwise     = error "dont"

instance Discrete Word16 where
    su x
        | x /= maxBound = x + 1
        | otherwise     = error "dont"
    pr x
        | x /= minBound = x - 1
        | otherwise     = error "dont"

instance Discrete Word32 where
    su x
        | x /= maxBound = x + 1
        | otherwise     = error "dont"
    pr x
        | x /= minBound = x - 1
        | otherwise     = error "dont"

instance Discrete Word64 where
    su x
        | x /= maxBound = x + 1
        | otherwise     = error "dont"
    pr x
        | x /= minBound = x - 1
        | otherwise     = error "dont"

