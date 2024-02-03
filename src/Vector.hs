{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Vector where

import TypeLevelPolynomial

data Vector (n :: Polynomial) a = Vector [a]
    deriving (Eq, Show)

scalar :: a -> Vector One a
scalar a = Vector [a]

append :: Vector m a -> Vector n a -> Vector (Add m n) a
append (Vector x) (Vector y) = Vector (x ++ y)

uncons :: Vector n a -> Maybe (a, Vector (Add n NegOne) a)
uncons (Vector []) = Nothing
uncons (Vector (x:xs)) = Just (x, Vector xs)

instance Num a => Num (Vector n a) where
    (+) (Vector x) (Vector y) = Vector (zipWith (+) x y)
    (-) (Vector x) (Vector y) = Vector (zipWith (-) x y)

mul :: Num a => Vector n a -> a -> Vector n a
mul (Vector v) a = Vector (map (* a) v)

div :: Fractional a => Vector n a -> a -> Vector n a
div (Vector v) a = Vector (map (\x -> x / a) v)

dot :: Num a => Vector n a -> Vector n a -> a
dot (Vector x) (Vector y) = sum $ zipWith (*) x y

norm :: Vector n Float -> Float
norm x = sqrt $ dot x x