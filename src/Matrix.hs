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

unconsRow :: Matrix m n a -> Maybe (Vector n a, Matrix (Add m ('Polynomial '[ '("1", 'Neg 1) ])) n a)
unconsRow (Matrix []) = Nothing
unconsRow (Matrix (x:xs)) = Just (Vector x, Matrix xs)

unconsCol :: Matrix m n a -> Maybe (Vector m a, Matrix m (Add n ('Polynomial '[ '("1", 'Neg 1) ])) a)
unconsCol (Matrix []) = Nothing
unconsCol (Matrix m) = Just (Vector [x | x:_ <- m], Matrix [xs | _:xs <- m])

transpose :: Matrix m n a -> Matrix n m a
transpose mat = case Matrix.unconsRow mat of
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

comp :: a -> Vector n a -> Vector m a -> Matrix m n a -> Matrix (Add One m) (Add One n) a
comp a00 a0j ai0 aij = appendRow (rowVector (append (scalar a00) a0j)) (appendCol (colVector ai0) aij)

luDecomp :: Matrix n n Float -> (Matrix n n Float, Matrix n n Float)
luDecomp (Matrix [[a]]) = (Matrix [[a]], Matrix [[1]])
luDecomp a = do
    let Just (a0j', aij') = unconsRow a
        Just (a00, a0j) = uncons a0j'
        Just (ai0, aij) = unconsCol aij'
    let l00 = a00
        l0j = Vector.mul a0j 0
        li0 = ai0
    let u00 = 1
        u0j = Vector.div a0j a00
        ui0 = Vector.mul ai0 0
    let (lij, uij) = luDecomp (aij - Matrix.mul (colVector li0) (rowVector u0j))
    let Matrix l = comp l00 l0j li0 lij
        Matrix u = comp u00 u0j ui0 uij
    (Matrix l, Matrix u)