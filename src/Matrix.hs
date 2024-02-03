{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Matrix where

import TypeLevelPolynomial
import Vector

data Matrix (n :: Polynomial) (m :: Polynomial) a = Matrix [[a]]
    deriving (Eq, Show)

rowVector :: Vector n a -> Matrix One n a
rowVector (Vector v) = Matrix [v]

colVector :: Vector n a -> Matrix n ('Polynomial '[ '("1", 'Pos 1) ]) a
colVector (Vector v) = Matrix [[vi] | vi <- v]

appendRow :: Matrix m1 n a -> Matrix m2 n a -> Matrix (Add m1 m2) n a
appendRow (Matrix x) (Matrix y) = Matrix (x ++ y)

appendCol :: Matrix m n1 a -> Matrix m n2 a -> Matrix m (Add n1 n2) a
appendCol (Matrix x) (Matrix y) = Matrix [xi ++ yi | (xi, yi) <- zip x y]

uncons :: Matrix m n a -> Maybe (Vector n a, Matrix (Add m ('Polynomial '[ '("1", 'Neg 1) ])) n a)
uncons (Matrix []) = Nothing
uncons (Matrix (x:xs)) = Just (Vector x, Matrix xs)

transpose :: Matrix m n a -> Matrix n m a
transpose mat = case Matrix.uncons mat of
    Nothing -> Matrix (repeat [])
    Just (v, m) ->
        let Matrix v' = colVector v
            Matrix m' = transpose m
        in Matrix [vi ++ mi | (vi, mi) <- zip v' m']

instance Num a => Num (Matrix m n a) where
    (+) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (+)) x y)
    (-) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (-)) x y)

mul :: Num a => Matrix l m a -> Matrix m n a -> Matrix l n a
mul (Matrix x) (Matrix y) = Matrix [mulVecMat xi y | xi <- x] where
    mulVecMat :: Num a => [a] -> [[a]] -> [a]
    mulVecMat xi y = sumVec [[xik * ykj | ykj <- yk] | (xik, yk) <- zip xi y] where
        sumVec :: Num a => [[a]] -> [a]
        sumVec = foldr (zipWith (+)) (repeat (fromInteger 0))