{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module HJ where

import Prelude as P hiding ((==), (+), (-), (/))
import qualified Prelude as P

import qualified Data.Vector as V
import Data.Foldable as F
import Data.Monoid
import GHC.Exts
import Data.Function
import Data.Bool

-- $setup
-- >>> :l
-- >>> :set -XOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import HJ
-- >>> import qualified Prelude as P

data Thing a = Scalar a | Arr (V.Vector a)
    deriving Functor

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
  (+) = zipWith' (P.+)
  (*) = zipWith' (P.*)
  (-) = zipWith' (P.-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Scalar . fromInteger

instance Fractional n => Fractional (Thing n) where
  fromRational = Scalar . fromRational
  (/) = zipWith' (P./)
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
  -- a == b = getAll . foldMap All $ zipWith' (==) a b

-- instance Ord n => Ord (Thing n) where
  -- Scalar a <= Scalar b = a <= b
  -- Scalar a <= Scalar b = a <= b
-- instance Real n => Real (Thing n) where
-- instance Enum n => Enum (Thing n) where
-- instance Integral n => Integral (Thing n) where


infixr 8 +
(+) :: Num n => Thing n -> Thing n -> Thing n
(+) = zipWith' (P.+)

infixr 8 /
(/) :: (Thing a -> Thing a -> Thing a) -> Thing a -> Thing a
f / a = F.foldl1 f . fmap Scalar $ a

infixr 8 -
(-) :: Num n => Thing n -> Thing n -> Thing n
(-) = zipWith' (P.-)

infixr 8 *
(*) :: Num n => Thing n -> Thing n -> Thing n
(*) = zipWith' (P.*)

infixr 8 %
(%) :: Fractional n => Thing n -> Thing n -> Thing n
(%) = zipWith' (P./)

infixr 8 ^
(^) :: (Fractional base, Integral pow) => Thing base -> Thing pow -> Thing base
(^) = zipWith' (^^)

infixr 8 ||
(||) :: Integral n => Thing n -> Thing n -> Thing n
(||) = zipWith' (flip mod)

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

>>> m# x
4
-}


(==) :: (Eq a, Num n) => Thing a -> Thing a -> Thing n
a == b = bool 0 1 <$> zipWith' (P.==) a b

(>) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a > b = bool 0 1 <$> zipWith' (P.>) a b

(<) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a < b = bool 0 1 <$> zipWith' (P.<) a b

(<=) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a <= b = bool 0 1 <$> zipWith' (P.<=) a b

(>=) :: (Ord a, Num n) => Thing a -> Thing a -> Thing n
a >= b = bool 0 1 <$> zipWith' (P.>=) a b

(#) :: Num n => () -> Thing a -> Thing n
() # x = Scalar . fromIntegral $ length x

m :: ()
m = ()





square :: Num n => Thing n -> Thing n
square = fmap (\n -> n P.* n)


zipWith' :: (a -> b -> c) -> Thing a -> Thing b -> Thing c
zipWith' f (Arr a) (Arr b) = Arr $ V.zipWith f a b
zipWith' f (Scalar a) (Arr b) = Arr $ fmap (f a) b
zipWith' f (Arr a) (Scalar b) = Arr $ fmap (flip f b) a
zipWith' f (Scalar a) (Scalar b) = Scalar (f a b)
