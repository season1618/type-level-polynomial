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