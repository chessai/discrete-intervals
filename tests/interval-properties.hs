{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import           Data.Functor.Identity
import           Data.Interval           (Interval (..))
import qualified Data.Interval           as I
import           Data.Proxy
import           Test.QuickCheck
import           Test.QuickCheck.Classes as QC

instance Monoid Int where
  mempty  = 0
  mappend = (+)
  mconcat = sum

instance (Enum a, Ord a, Arbitrary a) => Arbitrary (Interval a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ I.interval a b

main :: IO ()
main = do
  props
  --quickCheck prop_collapse

props :: IO ()
props = lawsCheckMany allPropsApplied

typeclassProps :: (Ord a, Eq a, Monoid a, Show a, Arbitrary a) => Proxy a -> [Laws]
typeclassProps p =
  [ QC.eqLaws p
  , QC.ordLaws p
  , QC.monoidLaws p 
  ]

allPropsApplied :: [(String,[Laws])]
allPropsApplied =
  [ ("Discrete Intervals",typeclassProps (Proxy :: Proxy (Interval Int)))
  ]
