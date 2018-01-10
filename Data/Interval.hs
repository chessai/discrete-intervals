{-# LANGUAGE BangPatterns #-}

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
import Data.Discrete
import Data.Semigroup (Semigroup(..))

-- | A Discrete Interval.
data Interval a = I !a !a | Empty

instance (Eq a, Discrete a, Ord a) => Eq (Interval a) where
  (==) = (==!)
  {-# INLINE (==) #-}

instance (Discrete a, Ord a) => Ord (Interval a) where
  compare (I a b) (I x y)
    = case compare a x of
        EQ -> compare b y
        r  -> r

instance (Discrete a, Ord a) => Semigroup (Interval a) where
  (<>) = hull
  {-# INLINE (<>) #-}

instance (Discrete a, Ord a, Monoid a) => Monoid (Interval a) where
  mempty = empty
  {-# INLINE mempty #-} 
  mappend = (<>)
  {-# INLINE mappend #-}

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

(+/-) :: (Discrete a, Num a, Ord a) => a -> a -> Interval a
a +/- b = a - b ... a + b
{-# INLINE (+/-) #-}

(...) :: (Discrete a, Ord a) => a -> a -> Interval a
(...) = interval
{-# INLINE (...) #-}

interval :: (Discrete a, Ord a) => a -> a -> Interval a
interval a b
  | a <= b = I a b
  | otherwise = I b a
{-# INLINE interval #-}

empty :: (Discrete a, Ord a) => Interval a
empty = Empty
{-# INLINE empty #-}

singleton :: (Discrete a, Ord a) => a -> Interval a
singleton a = a ... a
{-# INLINE singleton #-}

symmetric :: (Discrete a, Num a, Ord a) => a -> Interval a
symmetric x = negate x ... x
{-# INLINE symmetric #-}

inf :: (Discrete a, Ord a) => Interval a -> a 
inf (I a _) = a
{-# INLINE inf #-}

sup :: (Discrete a, Ord a) => Interval a -> a
sup (I _ b) = b
{-# INLINE sup #-}

valid :: (Discrete a, Ord a) => Interval a -> Bool
valid x = isNonEmpty x && inf x <= sup x
{-# INLINE valid #-}

invalid :: (Discrete a, Ord a) => Interval a -> Bool
invalid = not . valid
{-# INLINE invalid #-}

isEmpty :: (Discrete a, Ord a) => Interval a -> Bool
isEmpty x = x == Empty
{-# INLINE isEmpty #-}

isNonEmpty :: (Discrete a, Ord a) => Interval a -> Bool
isNonEmpty = not . isEmpty
{-# INLINE isNonEmpty #-}

singular :: (Discrete a, Ord a) => Interval a -> Bool
singular x = valid x && inf x == sup x
{-# INLINE singular #-}

width :: (Discrete a, Num a, Ord a) => Interval a -> a
width (I a b) = su $ b - a
{-# INLINE width #-}

toList :: (Enum a, Ord a) => Interval a -> [a]
toList (I a b) = [a..b]
{-# INLINE toList #-}

sortAsc :: (Discrete a, Ord a) => [Interval a] -> [Interval a]
sortAsc = L.sortBy go
  where
    go :: (Discrete a, Ord a) => Interval a -> Interval a -> Ordering
    go u v
      | inf u < inf v = LT
      | inf u > inf v = GT
      | otherwise     = EQ

sortDesc :: (Discrete a, Ord a) => [Interval a] -> [Interval a]
sortDesc = L.sortBy go
  where
    go :: (Discrete a, Ord a) => Interval a -> Interval a -> Ordering
    go u v
      | inf u < inf v = GT
      | inf u > inf v = LT
      | otherwise     = EQ

collapse :: (Discrete a, Ord a) => [Interval a] -> [Interval a]
collapse []  = []
collapse [x] = [x]
collapse !xs  = go (sortDesc xs)
  where
    go :: (Discrete a, Ord a) => [Interval a] -> [Interval a]
    go (x:y:ys) = if x ++? y then (x <> y) : collapse ys else x : collapse (y : ys)

member :: (Discrete a, Ord a) => a -> Interval a -> Bool
member x (I a b) = x >= a && x <= b
{-# INLINE member #-}

notMember :: (Discrete a, Ord a) => a -> Interval a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

hull :: (Discrete a, Ord a) => Interval a -> Interval a -> Interval a
hull x y
  | invalid x = y
  | invalid y = x 
  | otherwise = min (inf x) (inf y) ... max (sup x) (sup y)
{-# INLINE hull #-}

contains :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
contains x y = invalid y
            || (valid x && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

isSubsetOf :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

adjacent :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
adjacent x y = su (sup x) == inf y || su (sup y) == inf x
{-# INLINE adjacent #-}

overlaps :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
overlaps = (==?)
{-# INLINE overlaps #-}

mergeable :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
mergeable = (++?)
{-# INLINE mergeable #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: (Discrete a, Eq a, Ord a) => Interval a -> Interval a -> Bool
x ==! y = inf x == inf y && sup x == sup y
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: (Discrete a, Eq a, Ord a) => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
{-# INLINE (>=?) #-}

-- | Is @X@ adjacent to @Y@?
(||?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x ||? y = adjacent x y
{-# INLINE (||?) #-}

-- | Is @X@ mergeable (overlapping or adjacent) with @Y@?
(++?) :: (Discrete a, Ord a) => Interval a -> Interval a -> Bool
x ++? y = x ||? y || x ==? y
{-# INLINE (++?) #-}
