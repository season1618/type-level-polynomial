{-# LANGUAGE KindSignatures #-}

module Vector where

import TypeLevelPolynomial

data Vector (n :: Polynomial) a = Vector [a]
    deriving (Eq, Show)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append (Vector x) (Vector y) = Vector (x ++ y)

instance Num a => Num (Vector n a) where
    (+) (Vector x) (Vector y) = Vector (zipWith (+) x y)
    (-) (Vector x) (Vector y) = Vector (zipWith (-) x y)

dot :: Num a => Vector n a -> Vector n a -> a
dot (Vector x) (Vector y) = sum $ zipWith (*) x y

norm :: Vector n Float -> Float
norm x = sqrt $ dot x x