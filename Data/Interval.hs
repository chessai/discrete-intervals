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
  , singular
  , member
  , notMember

    -- * Conversion 
  , toList
 
    -- * Ordering 
  , contains
  , isSubsetOf
  , adjacent 
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
  ) where 

import Data.Semigroup (Semigroup(..))

-- | A Discrete Interval.
data Interval a = I !a !a | Empty
  deriving (Eq)

instance (Ord a) => Ord (Interval a) where
  compare (I a b) (I x y)
    = case compare a x of
        EQ -> compare b y
        r  -> r

instance (Enum a, Ord a) => Semigroup (Interval a) where
  (<>) = hull
  {-# INLINE (<>) #-}

instance (Enum a, Ord a) => Monoid (Interval a) where
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

(+/-) :: (Enum a, Num a, Ord a) => a -> a -> Interval a
a +/- b = a - b ... a + b
{-# INLINE (+/-) #-}

(...) :: (Enum a, Ord a) => a -> a -> Interval a
(...) = I
{-# INLINE (...) #-}

interval :: (Enum a, Ord a) => a -> a -> Interval a
interval a b
  | a <= b = I a b
  | otherwise = I b a
{-# INLINE interval #-}

empty :: (Enum a, Ord a) => Interval a
empty = Empty
{-# INLINE empty #-}

singleton :: (Enum a, Ord a) => a -> Interval a
singleton a = a ... a
{-# INLINE singleton #-}

inf :: (Enum a, Ord a) => Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

sup :: (Enum a, Ord a) => Interval a -> a
sup (I _ b) = b
{-# INLINE sup #-}

valid :: (Enum a, Ord a) => Interval a -> Bool
valid x = inf x <= sup x
{-# INLINE valid #-}

invalid :: (Enum a, Ord a) => Interval a -> Bool
invalid = not . valid
{-# INLINE invalid #-}

isEmpty :: (Enum a, Ord a) => Interval a -> Bool
isEmpty x = x == Empty
{-# INLINE isEmpty #-}

singular :: (Enum a, Ord a) => Interval a -> Bool
singular x = valid x && inf x == sup x
{-# INLINE singular #-}

toList :: (Enum a, Ord a) => Interval a -> [a]
toList (I a b) = [a..b]
{-# INLINE toList #-}

member :: (Enum a, Ord a) => a -> Interval a -> Bool
member x (I a b) = x >= a && x <= b
{-# INLINE member #-}

notMember :: (Enum a, Ord a) => a -> Interval a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

hull :: (Enum a, Ord a) => Interval a -> Interval a -> Interval a
hull x y
  | (invalid x || isEmpty x) = y
  | (invalid y || isEmpty y) = x 
  | otherwise = min (inf x) (inf y) ... max (sup x) (sup y)
{-# INLINE hull #-}

contains :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
contains x y = (invalid y)
            || (valid x && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

isSubsetOf :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

adjacent :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
adjacent x y = succ (sup x) == inf y || succ (sup y) == inf x
{-# INLINE adjacent #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: (Enum a, Eq a, Ord a) => Interval a -> Interval a -> Bool
x ==! y = inf x == inf y && sup x == sup y
{-# INLINE (==!) #-}


-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: (Enum a, Eq a, Ord a) => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) ::(Enum a, Ord a)=> Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
{-# INLINE (>=?) #-}

width :: (Enum a, Num a, Ord a) => Interval a -> a
width (I a b) = succ $ b - a
{-# INLINE width #-}

symmetric :: (Enum a, Num a, Ord a) => a -> Interval a
symmetric x = negate x ... x
{-# INLINE symmetric #-}
