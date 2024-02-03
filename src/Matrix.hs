{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Matrix where

import TypeLevelPolynomial
import Vector

data Matrix (n :: Polynomial) (m :: Polynomial) a = Matrix [[a]]
    deriving (Eq, Show)

appendRow :: Matrix n1 m a -> Matrix n2 m a -> Matrix (Add n1 n2) m a
appendRow (Matrix x) (Matrix y) = Matrix (x ++ y)

appendCol :: Matrix n m1 a -> Matrix n m2 a -> Matrix n (Add m1 m2) a
appendCol (Matrix x) (Matrix y) = Matrix [xi ++ yi | (xi, yi) <- zip x y]

uncons :: Matrix m n a -> Maybe (Vector n a, Matrix (Add m ('Polynomial '[ '("1", 'Neg 1) ])) n a)
uncons (Matrix []) = Nothing
uncons (Matrix (x:xs)) = Just (Vector x, Matrix xs)

instance Num a => Num (Matrix n m a) where
    (+) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (+)) x y)
    (-) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (-)) x y)

mul :: Num a => Matrix l m a -> Matrix m n a -> Matrix l n a
mul (Matrix x) (Matrix y) = Matrix [mulVecMat xi y | xi <- x] where
    mulVecMat :: Num a => [a] -> [[a]] -> [a]
    mulVecMat xi y = sumVec [[xik * ykj | ykj <- yk] | (xik, yk) <- zip xi y] where
        sumVec :: Num a => [[a]] -> [a]
        sumVec = foldr (zipWith (+)) (repeat (fromInteger 0))