{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module MassivHJ where

import Prelude as P hiding
    ((==), (+), (-), (/), (>), (<), (>=), (<=)
    , zipWith, foldl, (<$>), map, length
    )
import qualified Prelude as P
import qualified Data.Massiv.Array as M

import Data.Monoid
import GHC.Exts
import Data.Function
import Data.Bool
import Data.Maybe

-- $setup
-- >>> :l
-- >>> :set -XOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import HJ
-- >>> import qualified Prelude as P

data Thing r ix a = Arr (M.Array r ix a)

-- instance Functor (Thing r ix) where
--   fmap f =

-- instance (M.Index ix, Semigroup a) => Semigroup (Thing r ix a) where
--   (<>) = zipWith (P.<>)

-- instance (M.Index ix) => Applicative (Thing r ix) where
--     pure = Scalar

--     (Arr f) <*> (Arr a) = Arr $ M.liftArray2 ($) f a
--     (Scalar f) <*> (Arr a) = Arr $ fmap f a
--     (Arr f) <*> (Scalar a) = Arr $ fmap (($ a)) f
--     (Scalar f) <*> (Scalar a) = Scalar (f a)

fromList' :: (M.Construct r Int a) =>  [a] -> Thing r M.Ix1 a
fromList' xs = Arr $ M.makeArray M.Seq (M.Sz (P.length xs)) (xs !!)

singleton :: M.Construct r ix a => a -> Thing r ix a
singleton = Arr . M.singleton

empty :: M.Construct r ix a => Thing r ix a
empty = Arr M.empty

-- instance (M.Index ix) => Alternative (Thing r ix) where
--   empty = Arr M.empty
--   Scalar x <|> Scalar y = fromList' [x, y]
--   Arr x <|> Scalar y = Arr (x <> pure y)
--   Scalar x <|> Arr y = Arr (pure x <> y)
--   Arr x <|> Arr y = Arr (x <> y)

instance (Show a, Show (M.Array r ix a)) => Show (Thing r ix a) where
  show (Arr v) = show v

-- instance Foldable (Thing r ix) where
--   foldMap f (Scalar a) = f a
--   foldMap f (Arr v) = M.foldMono f v

instance M.Construct r Int a => IsList (Thing r M.Ix1 a) where
    type Item (Thing r M.Ix1 a) = a
    fromList xs = (fromList' xs)
    -- toList = F.toList

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
instance (M.Index ix, Num n) => Num (Thing M.D ix n) where
  (+) = zipWith (P.+)
  (*) = zipWith (P.*)
  (-) = zipWith (P.-)
  abs = map abs
  signum = map signum
  fromInteger = singleton . fromInteger

instance (M.Construct M.D ix n, M.Index ix, Num (Thing M.D ix n), Fractional n) => Fractional (Thing M.D ix n) where
  fromRational = singleton . fromRational
  (/) = zipWith (P./)
  recip = map recip

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
-- instance Eq n => Eq (Thing r ix n) where
  -- a == b = getAll . foldMap All $ zipWith (==) a b

-- instance Ord n => Ord (Thing r ix n) where
  -- Scalar a <= Scalar b = a <= b
  -- Scalar a <= Scalar b = a <= b
-- instance Real n => Real (Thing r ix n) where
-- instance Enum n => Enum (Thing r ix n) where
-- instance Integral n => Integral (Thing r ix n) where


zipWith :: (M.Source r ix a, M.Source r ix b) => (a -> b -> c) -> Thing r ix a -> Thing r ix b -> Thing M.D ix c
zipWith f (Arr a) (Arr b) = Arr (M.liftArray2 f a b)

map :: (M.Source r ix a) => (a -> b) -> Thing r ix a -> Thing M.D ix b
map f (Arr xs) = Arr (M.map f xs)

(<$>) :: (M.Source r ix a) => (a -> b) -> Thing r ix a -> Thing M.D ix b
(<$>) f (Arr xs) = Arr (M.map f xs)

length (Arr xs) = M.elemsCount xs




infixr 8 +
(+) :: (Num n, M.Source r ix n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(+) = zipWith (P.+)

infixr 8 /
(/) :: (M.Construct r ix a, M.Source r ix a) => (Thing r ix a -> Thing r ix a -> Thing r ix a) -> Thing r ix a -> Thing r ix a
f / a = fromMaybe empty $ foldl f a

foldl f (Arr xs) = M.foldlS go Nothing $ xs
  where
    go Nothing e = Just $ singleton e
    go (Just a) e = Just . f a $ singleton e

infixr 8 -
(-) :: (M.Source r ix n, Num n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(-) = zipWith (P.-)

infixr 8 *
(*) :: (M.Source r ix n, Num n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(*) = zipWith (P.*)

infixr 8 %
(%) :: (M.Source r ix n, Fractional n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(%) = zipWith (P./)

infixr 8 ^
(^) :: (M.Source r ix base, M.Source r ix pow, Fractional base, Integral pow) => Thing r ix base -> Thing r ix pow -> Thing M.D ix base
(^) = zipWith (^^)

infixr 8 ||
(||) :: (M.Source r ix n, Integral n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(||) = zipWith (flip mod)

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

>>>  m|>. ([-1.7, 1, 1.7] :: Thing r ix P.Float)
[-1,1,2]

>>> 3 >. [1, 3, 5]
[3,3,5]

>>> (>.) / [1, 6, 5]
6

-- increment
>>> m|>: [-2, 3, 5, 6.3]
[-1.0,4.0,6.0,7.3]

-- larger or equal
>>>  3 >: [1, 3, 5]
[1,1,0]


-}


(==) :: (M.Source r ix a, Eq a, Num n) => Thing r ix a -> Thing r ix a -> Thing M.D ix n
a == b = bool 0 1 <$> zipWith (P.==) a b

(>) :: (M.Source r ix a, Ord a, Num n) => Thing r ix a -> Thing r ix a -> Thing M.D ix n
a > b = bool 0 1 <$> zipWith (P.>) a b

(<) :: (M.Source r ix a, Ord a, Num n) => Thing r ix a -> Thing r ix a -> Thing M.D ix n
a < b = bool 0 1 <$> zipWith (P.<) a b

(<=) :: (M.Source r ix a, Ord a, Num n) => Thing r ix a -> Thing r ix a -> Thing M.D ix n
a <= b = bool 0 1 <$> zipWith (P.<=) a b

(>=) :: (M.Source r ix a, Ord a, Num n) => Thing r ix a -> Thing r ix a -> Thing M.D ix n
a >= b = bool 0 1 <$> zipWith (P.>=) a b

(|#) :: (M.Load r ix a, M.Construct r ix n, Num n) => () -> Thing r ix a -> Thing r ix n
() |# x = singleton . fromIntegral $ length x

(#) :: (M.Source r ix n, Ord n, Num n) => Thing r ix n -> Thing M.D ix a -> Thing M.D ix a
predicates # xs =
    map snd . filterThing fst
    $ zipWith (,) ((P.> 0) <$> predicates) xs

-- ceil
(|>.) :: (M.Source r ix a, RealFrac a, Integral b) => () -> Thing r ix a -> Thing M.D ix b
() |>. a = ceiling <$> a

-- max
(>.) :: (M.Source r ix n, Ord n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
(>.) = zipWith max

-- increment
(|>:) :: (M.Source r ix n, Num n) => () -> Thing r ix n -> Thing M.D ix n
() |>: a = map (P.+1) a

-- >=
(>:) :: (M.Source r ix n, Ord n, Num n) => Thing r ix n -> Thing r ix n -> Thing M.D ix n
a >: b = boolNum <$> zipWith (P.>=) a b









filterThing :: (a -> Bool) -> Thing r ix a -> Thing r ix a
filterThing  f = asum . fmap embed
  where
    embed a | f a = pure a
            | otherwise = empty

square :: Num n => Thing r ix n -> Thing r ix n
square = fmap (\n -> n P.* n)

m :: ()
m = ()




