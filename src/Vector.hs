{-# LANGUAGE KindSignatures #-}

module Vector where

import TypeLevelPolynomial

data Vector (n :: Polynomial) a = Vector [a]
    deriving (Eq, Show)

add :: Num a => Vector n a -> Vector n a -> Vector n a
add (Vector x) (Vector y) = Vector (zipWith (+) x y)