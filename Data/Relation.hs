{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relation where

import Control.Lens.Fold
import Prelude hiding (id, Num(..))
import qualified Prelude as P
import Data.Foldable (foldlM)
import Data.Interval
import Data.Maybe
import Data.Ring
import Data.Semiring

newtype Rel = Rel { getRel :: Bool }
  deriving (Eq, Semiring, Read, Ring, Show)

-- interval calculus
id,lt,gt,m,mi,o,oi,s,si,d,di,f,fi,eq
  :: (Enum a, Ord a, Semiring a)
  => Interval a
  -> Interval a
  -> Rel
id _ _ = Rel $ True 
lt x y = Rel $ sup x < inf y
gt x y = Rel $ inf x > sup y
m  x y = Rel $ succ (sup x) == inf y || succ (sup y) == inf x
mi x y = Rel $ succ (sup y) == inf x || succ (sup x) == inf y
o  x y = Rel $ inf x <= sup y && sup x >= inf y
oi x y = Rel $ inf y <= sup x && sup y >= inf x
s  x y = Rel $ inf x == inf y && sup y >= sup x
si x y = Rel $ inf y == inf x && sup x >= sup y
d  x y = Rel $ inf y <= inf x && sup y >= sup x
di x y = Rel $ inf x <= inf y && sup x >= sup y
f  x y = Rel $ inf x >= inf y && sup x == sup y
fi x y = Rel $ inf y >= inf x && sup y == sup x
eq x y = Rel $ inf x == inf y && sup x == sup y

mergeableL :: (Enum a, Ord a, Semiring a) => Interval a -> Interval a -> Rel
mergeableL x y = m x y + o x y

mergeableR :: (Enum a, Ord a, Semiring a) => Interval a -> Interval a -> Rel
mergeableR x y = mi x y + oi x y

-- assume sorted in ascending order
-- BORING list collapse
coll :: (Enum a, Ord a, Semiring a) => [Interval a] -> [Interval a]
coll = coll' (:)

--coll []  = []
--coll [x] = [x]
--coll (x:y:ys)
--  | getRel $ mergeableL x y = coll ((x <> y) : ys)
--  | otherwise = x : coll (y : ys)

-- super amazing GENERALISED FOLDABLE cOLLAPSE
-- (((POWER)))
coll'
  :: forall a t.
     (Applicative t,
     Foldable t,
     Semiring (t (Interval a)),
     Enum a,
     Ord a,
     Semiring a)
  => (Interval a -> t (Interval a) -> t (Interval a)) -- ^ cons
  -> t (Interval a)                                   -- ^ structure containing potentially unmerged intervals
  -> t (Interval a)                                   -- ^ collapsed structure
coll' cons xs
  | null xs = pure zero
  | length xs == 1 = xs
  | otherwise = go xs
  where
    go ys
      | getRel $ mergeableL x y = coll' cons ((x <> y) `cons` ((delete cons x ys) + (delete cons y ys)))
      | otherwise               = x `cons` coll' cons (y `cons` ys)
    x = fromJust $ get 0 xs
    y = fromJust $ get 1 xs

deleteL :: forall a. a -> [a] -> [a]
deleteL a xs = foldr f (const []) xs False
  where
    f :: a -> (Bool -> [a]) -> (Bool -> [a])
    f x g found
      | x == a && not found = g True
      | otherwise           = x : g found

-- ^ Delete an element from a foldable structure.
delete
  :: forall a t.
     (Foldable t, Eq a, Semiring (t a))
  => (a -> t a -> t a) -- ^ cons
  -> a                 -- ^ element to delete
  -> t a               -- ^ structure containing element
  -> t a
delete cons a xs = foldr f (const zero) xs False
  where
    f :: a -> (Bool -> t a) -> (Bool -> t a)
    f x g found
      | x == a && not found = g True
      | otherwise           = x `cons` g found

deleteN :: (Foldable f, Semiring (f a)) => (a -> f a -> f a) -> Int -> f a -> f a
deleteN cons n xs = flipTfo xs $ folded . ifiltered (\i _ -> i /= n)
  where
    flipTfo = flip toFoldableOf
    toFoldableOf l = foldrOf l cons zero

deleteList :: Semiring a => Int -> [a] -> [a]
deleteList = deleteN (:)

-- ^ access the nth element of a foldable structure.
get :: (Foldable t) => Int -> t a -> Maybe a
get n = either pure (\_ -> Nothing) . foldlM hk n
  where
    hk 0 x = Left x
    hk p _ = Right (p - 1)

a,b,c,e,k :: Interval Int
a = interval 2 4
b = interval 5 17
c = interval 13 12
e = interval 9 10
k = interval 18 12

testIs :: [Interval Int]
testIs = [a,b,c,e,k]

testIs2 = [a,e,k]

test :: IO Bool
test = pure $
    ((coll testIs) == [(interval 2 18)])
 && ((coll testIs2) == [interval 2 4, interval 9 10, interval 12 18])
