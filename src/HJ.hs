{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module HJ where

import Prelude as P hiding
    ((==), (+), (-), (/), (>), (<), (>=), (<=)
    , zipWith
    )
import qualified Prelude as P

import qualified Data.Vector as V
import Data.Foldable as F
import Data.Monoid
import GHC.Exts
import Data.Function
import Data.Bool
import Control.Applicative

-- $setup
-- >>> :l
-- >>> :set -XOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import HJ
-- >>> import qualified Prelude as P

data Thing a = Scalar a | Arr (V.Vector a)
    deriving Functor

instance Semigroup a => Semigroup (Thing a) where
  (<>) = liftA2 (P.<>)

instance Applicative Thing where
    pure = Scalar

    (Arr f) <*> (Arr a) = Arr $ V.zipWith ($) f a
    (Scalar f) <*> (Arr a) = Arr $ fmap f a
    (Arr f) <*> (Scalar a) = Arr $ fmap (($ a)) f
    (Scalar f) <*> (Scalar a) = Scalar (f a)

instance Alternative Thing where
  empty = Arr []
  Scalar x <|> Scalar y = Arr [x, y]
  Arr x <|> Scalar y = Arr (x <> pure y)
  Scalar x <|> Arr y = Arr (pure x <> y)
  Arr x <|> Arr y = Arr (x <> y)

instance Show a => Show (Thing a) where
  show (Scalar a) = show a
  show (Arr v) = show v

instance Foldable Thing where
  foldMap f (Scalar a) = f a
  foldMap f (Arr v) = foldMap f v

instance IsList (Thing a) where
    type Item (Thing a) = a
    fromList xs = Arr (V.fromList xs)
    toList = F.toList

{- | Numeric operations
>>> 1 + 2
3

>>> 1 + 2
3

>>> 2 * 3
6

>>> 3 - 2
1

>>> 2 - 3
-1

>>> 2 ^ 3
8.0

>>> square 3
9


>>> square [1, 2, 3, 4]
[1,4,9,16]

>>> [1, 2, 3] + [10, 20, 30]
[11,22,33]

>>> 1 + [10, 20, 30]
[11,21,31]

>>> [1, 2, 3] + 10
[11,12,13]

>>> 2 || [0, 1, 2, 3, 4, 5, 6, 7]
[0,1,0,1,0,1,0,1]

>>> (2+1)*(2+2)
12
-}
instance Num n => Num (Thing n) where
  (+) = liftA2 (P.+)
  (*) = liftA2 (P.*)
  (-) = liftA2 (P.-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Scalar . fromInteger

instance Fractional n => Fractional (Thing n) where
  fromRational = Scalar . fromRational
  (/) = liftA2 (P./)
  recip = fmap recip

{-
>>> (+) / [2, 3, 4]
9
>>> (*) / [2, 3, 4]
24
-}

-- {-|
-- >>> 1 == 1
-- True

-- >>> 2 == 1
-- False

-- >>> [1] == [1]
-- True
-- >>> [1] == [1, 1]
-- True
-- >>> [1] == [1, 2]
-- False
-- >>> 1 == [1, 1, 1]
-- True
-- -}
-- instance Eq n => Eq (Thing n) where
  -- a == b = getAll . foldMap All $ zipWith (==) a b

-- instance Ord n => Ord (Thing n) where
  -- Scalar a <= Scalar b = a <= b
  -- Scalar a <= Scalar b = a <= b
-- instance Real n => Real (Thing n) where
-- instance Enum n => Enum (Thing n) where
-- instance Integral n => Integral (Thing n) where


infixr 8 +
(+) :: Num n => Thing n -> Thing n -> Thing n
(+) = liftA2 (P.+)

infixr 8 /
(/) :: (Thing a -> Thing a -> Thing a) -> Thing a -> Thing a
f / a = F.foldl1 f . fmap Scalar $ a

infixr 8 -
(-) :: Num n => Thing n -> Thing n -> Thing n
(-) = liftA2 (P.-)

infixr 8 *
(*) :: Num n => Thing n -> Thing n -> Thing n
(*) = liftA2 (P.*)

infixr 8 %
(%) :: Fractional n => Thing n -> Thing n -> Thing n
(%) = liftA2 (P./)

infixr 8 ^
(^) :: (Fractional base, Integral pow) => Thing base -> Thing pow -> Thing base
(^) = liftA2 (^^)

infixr 8 ||
(||) :: Integral n => Thing n -> Thing n -> Thing n
(||) = liftA2 (flip mod)

boolNum :: Num n => Bool -> n
boolNum = bool 0 1


{-|
>>> 2 > 1
1

>>> 2 == 1
0

>>>  2 < 1
0

>>> let x = [5, 4, 1, 9]
>>> x > 2
[1,1,0,1]

-- Are all things greater than 2?
>>> (*) / x > 2
0

-- How many things are greater than 2?
>>> (+) / x > 2
3

>>> x == x
[1,1,1,1]

>>> (+) / x == x
4

>>> m|# x
4

>>> let y = [6, 7, 8, 9, 10]
>>> [1, 1, 0, 1, 0] # y
[6,7,9]

>>> (y > 7) # y
[8,9,10]

>>>  m|>. ([-1.7, 1, 1.7] :: Thing P.Float)
[-1,1,2]

>>> 3 >. [1, 3, 5]
[3,3,5]

>>> (>.) / [1, 6, 5]
6

-- increment
>>> m|>: [-2, 3, 5, 6.3]
[-1,4,6,7.3]
-}


(==) :: (Eq a, Num n) => Thing a -> Thing a -> Thing n
a == b = bool 0 1 <$> liftA2 (P.==) a b

(>) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a > b = bool 0 1 <$> liftA2 (P.>) a b

(<) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a < b = bool 0 1 <$> liftA2 (P.<) a b

(<=) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a <= b = bool 0 1 <$> liftA2 (P.<=) a b

(>=) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a >= b = bool 0 1 <$> liftA2 (P.>=) a b

(|#) :: Num n => () -> Thing a -> Thing n
() |# x = Scalar . fromIntegral $ length x

(#) :: (Ord n, Num n) => Thing n -> Thing a -> Thing a
predicates # xs =
    fmap snd . filterThing fst
    $ liftA2 (,) ((P.> 0) <$> predicates) xs

-- ceil
(|>.) :: (RealFrac a, Integral b) => () -> Thing a -> Thing b
() |>. a = ceiling <$> a

-- max
(>.) :: Ord n => Thing n -> Thing n -> Thing n
(>.) = liftA2 max

-- increment
(|>:) :: Num n => () -> Thing n -> Thing n
() |>: a = fmap (P.+1) a

-- >=
(>:) :: (Ord n, Num n) => Thing n -> Thing n -> Thing n
a >: b = boolNum <$> liftA2 (P.>=) a b









filterThing :: (a -> Bool) -> Thing a -> Thing a
filterThing  f = asum . fmap embed
  where
    embed a | f a = pure a
            | otherwise = empty

square :: Num n => Thing n -> Thing n
square = fmap (\n -> n P.* n)

m :: ()
m = ()




