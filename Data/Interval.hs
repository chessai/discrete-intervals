{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Interval
  ( -- * Interval type 
    Interval(..)
    
    -- * Construction
  , (...)
  , (+/-)
  , interval
  , empty
  , singleton
  , symmetric 
  
    -- * Combine
  , hull

    -- * Querying 
  , inf
  , sup
  , width 
  , valid
  , invalid
  , isEmpty
  , isNonEmpty 
  , singular
  , member
  , notMember

    -- * Conversion 
  , toList
 
    -- * Ordering 
  , contains
  , isSubsetOf
  , adjacent
  , mergeable
  , overlaps
  , sortAsc
  , sortDesc
  , collapse 
  , (<!)
  , (<=!)
  , (==!)
  , (/=!)
  , (>!)
  , (>=!)
  , (<?)
  , (<=?)
  , (==?)
  , (/=?)
  , (>?) 
  , (>=?)
  , (||?)
  , (++?)
  ) where 

import qualified Data.List as L
import Prelude hiding (Num(..))
import Data.Data
import Data.Typeable
import Data.Semiring
import Data.Ring
import GHC.Generics

-- | A Enum Interval.
data Interval a
  = Empty
  | I !a !a
  deriving (Data, Typeable, Generic, Generic1)

instance (Eq a, Enum a, Semiring a, Ord a) => Eq (Interval a) where
  (==) = (==!)
  {-# INLINE (==) #-}

instance (Enum a, Semiring a, Ord a) => Ord (Interval a) where
  compare (I a b) (I x y)
    = case compare a x of
        EQ -> compare b y
        r  -> r

instance (Enum a, Semiring a, Ord a) => Semigroup (Interval a) where
  (<>) = hull
  {-# INLINE (<>) #-}

instance (Enum a, Semiring a, Ord a, Monoid a) => Monoid (Interval a) where
  mempty = empty
  {-# INLINE mempty #-} 
  mappend = (<>)
  {-# INLINE mappend #-}

instance (Enum a, Semiring a, Ord a, Semiring a) => Semiring (Interval a) where
  zero = empty
  one  = interval one one
  plus (I a b) (I x y) = interval (a + x) (b + y)
  times (I a b) (I x y) = interval (a * x) (b * y)

instance (Enum a, Semiring a, Ord a, Ring a) => Ring (Interval a) where
  negate (I x y) = interval (negate x) (negate y)

instance Functor Interval where
  fmap _ Empty   = Empty
  fmap f (I a b) = I (f a) (f b)

instance Foldable Interval where
  foldMap _ Empty   = mempty 
  foldMap f (I a b) = f a `mappend` f b

instance Traversable Interval where
  traverse _ Empty   = pure Empty 
  traverse f (I a b) = I <$> f a <*> f b

instance Applicative Interval where
  pure a = I a a
  I f g <*> I a b = I (f a) (g b)

instance Monad Interval where
  I a b >>= f = I a' b' where
    I a' _ = f a
    I _ b' = f b

instance Show a => Show (Interval a) where
  showsPrec _ Empty =
    showString " Empty "
  showsPrec n (I a b) =
    showParen (n > 3) $
      showsPrec 3 a .
      showString " ... " .
      showsPrec 3 b

infix  3 ...
infixl 6 +/-

(+/-) :: (Enum a, Ring a, Semiring a, Ord a) => a -> a -> Interval a
a +/- b = a - b ... a + b
{-# INLINE (+/-) #-}

(...) :: (Enum a, Semiring a, Ord a) => a -> a -> Interval a
(...) = interval
{-# INLINE (...) #-}

interval :: (Enum a, Semiring a, Ord a) => a -> a -> Interval a
interval a b
  | a <= b    = I a b
  | otherwise = I b a
{-# INLINE interval #-}

empty :: (Enum a, Semiring a, Ord a) => Interval a
empty = Empty
{-# INLINE empty #-}

singleton :: (Enum a, Semiring a, Ord a) => a -> Interval a
singleton a = a ... a
{-# INLINE singleton #-}

symmetric :: (Enum a, Ring a, Semiring a, Ord a) => a -> Interval a
symmetric x = negate x ... x
{-# INLINE symmetric #-}

inf :: (Enum a, Semiring a, Ord a) => Interval a -> a 
inf Empty   = zero
inf (I a _) = a
{-# INLINE inf #-}

sup :: (Enum a, Semiring a, Ord a) => Interval a -> a
sup Empty   = zero
sup (I _ b) = b
{-# INLINE sup #-}

valid :: (Enum a, Semiring a, Ord a) => Interval a -> Bool
valid x = isNonEmpty x && inf x <= sup x
{-# INLINE valid #-}

invalid :: (Enum a, Semiring a, Ord a) => Interval a -> Bool
invalid = not . valid
{-# INLINE invalid #-}

isEmpty :: (Enum a, Semiring a, Ord a) => Interval a -> Bool
isEmpty x = x == Empty
{-# INLINE isEmpty #-}

isNonEmpty :: (Enum a, Semiring a, Ord a) => Interval a -> Bool
isNonEmpty = not . isEmpty
{-# INLINE isNonEmpty #-}

singular :: (Enum a, Semiring a, Ord a) => Interval a -> Bool
singular x = valid x && inf x == sup x
{-# INLINE singular #-}

width :: (Enum a, Ring a, Semiring a, Ord a) => Interval a -> a
width (I a b) = succ $ b - a
{-# INLINE width #-}

toList :: (Enum a, Semiring a, Ord a) => Interval a -> [a]
toList (I a b) = [a..b]
{-# INLINE toList #-}

fromList :: (Enum a, Semiring a, Ord a) => [a] -> Interval a
fromList [] = Empty
fromList xs = interval (head sxs) (last sxs)
  where
    sxs = L.sort xs

sortAsc :: (Enum a, Semiring a, Ord a) => [Interval a] -> [Interval a]
sortAsc = L.sortBy go
  where
    go :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Ordering
    go u v
      | inf u < inf v = LT
      | inf u > inf v = GT
      | otherwise     = EQ

sortDesc :: (Enum a, Semiring a, Ord a) => [Interval a] -> [Interval a]
sortDesc = L.sortBy go
  where
    go :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Ordering
    go u v
      | inf u < inf v = GT
      | inf u > inf v = LT
      | otherwise     = EQ

collapse :: (Enum a, Semiring a, Ord a) => [Interval a] -> [Interval a]
collapse []  = []
collapse [x] = [x]
collapse !xs  = go (sortDesc xs)
  where
    go :: (Enum a, Semiring a, Ord a) => [Interval a] -> [Interval a]
    go (x:y:ys) = if x ++? y then (x <> y) : collapse ys else x : collapse (y : ys)

member :: (Enum a, Semiring a, Ord a) => a -> Interval a -> Bool
member x (I a b) = x >= a && x <= b
{-# INLINE member #-}

notMember :: (Enum a, Semiring a, Ord a) => a -> Interval a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

hull :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Interval a
hull Empty y = y
hull x Empty = x
hull x y
  | invalid x = y
  | invalid y = x 
  | otherwise = min (inf x) (inf y) ... max (sup x) (sup y)
{-# INLINE hull #-}

contains :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
contains x y = invalid y
            || (valid x && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

isSubsetOf :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

adjacent :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
adjacent x y = succ (sup x) == inf y || succ (sup y) == inf x
{-# INLINE adjacent #-}

overlaps :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
overlaps = (==?)
{-# INLINE overlaps #-}

mergeable :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
mergeable = (++?)
{-# INLINE mergeable #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: (Enum a, Eq a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x ==! y = inf x == inf y && sup x == sup y
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: (Enum a, Eq a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
{-# INLINE (>=?) #-}

-- | Is @X@ adjacent to @Y@?
(||?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x ||? y = adjacent x y
{-# INLINE (||?) #-}

-- | Is @X@ mergeable (overlapping or adjacent) with @Y@?
(++?) :: (Enum a, Semiring a, Ord a) => Interval a -> Interval a -> Bool
x ++? y = x ||? y || x ==? y
{-# INLINE (++?) #-}
