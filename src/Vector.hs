{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Vector where

import TypeLevelPolynomial

data Vector (n :: Polynomial) a = Vector [a]
    deriving Eq

instance Show a => Show (Vector n a) where
    show (Vector v) = "(" ++ showList v ++ ")" where
        showList :: Show a => [a] -> String
        showList [] = ""
        showList [a] = show a
        showList (a:as) = show a ++ ", " ++ showList as

zero :: Vector n Float -> Vector n Float
zero v = mul v 0

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
div (Vector v) a = Vector (map (/ a) v)

dot :: Num a => Vector n a -> Vector n a -> a
dot (Vector x) (Vector y) = sum $ zipWith (*) x y

norm :: Vector n Float -> Float
norm v = sqrt $ dot v v

normalize :: Vector n Float -> Vector n Float
normalize v = Vector.div v (norm v)

orthonormalize :: [Vector m Float] -> [Vector m Float] -> [Vector m Float]
orthonormalize e [] = e
orthonormalize e (v:vs) = do
    let v' = normalize $ v - foldr (+) (zero v) [Vector.mul ei (dot ei v) | ei <- e]
    orthonormalize (e ++ [v']) vs