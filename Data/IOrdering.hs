module Data.IOrdering
  ( IOrdering(..)
  ) where

import Data.Semigroup (Semigroup(..))

data IOrdering = L | E | G | B | OPos | ONeg | O
  deriving (Eq)

instance Monoid IOrdering where
  mempty = E
  {-# INLINE mempty #-} 
  mappend = (<>)
  {-# INLINE mappend #-}

instance Semigroup IOrdering where
  OPos <> ONeg = O
  OPos <> OPos = O
  ONeg <> ONeg = O
  ONeg <> OPos = O
  OPos <> _    = OPos
  ONeg <> _    = ONeg
  _    <> OPos = OPos
  _    <> ONeg = ONeg

  L <> G = B
  L <> L = L
  
  G <> L = B
  G <> G = G
  E <> k = k
  {-# INLINE (<>) #-}
