{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Matrix where

import TypeLevelPolynomial
import Vector

data Matrix (n :: Polynomial) (m :: Polynomial) a = Matrix [[a]]
    deriving (Eq, Show)

id :: Matrix n n Float -> Matrix n n Float
id (Matrix [[a]]) = Matrix [[1]]
id a = do
    let Just (a0j', aij') = unconsRow a
        Just (a00, a0j) = uncons a0j'
        Just (ai0, aij) = unconsCol aij'
    let (Matrix x) = comp 1 (Vector.mul a0j 0) (Vector.mul ai0 0) (Matrix.id aij)
    Matrix x

rowVector :: Vector n a -> Matrix One n a
rowVector (Vector v) = Matrix [v]

colVector :: Vector n a -> Matrix n One a
colVector (Vector v) = Matrix [[vi] | vi <- v]

appendRow :: Matrix m1 n a -> Matrix m2 n a -> Matrix (Add m1 m2) n a
appendRow (Matrix x) (Matrix y) = Matrix (x ++ y)

appendCol :: Matrix m n1 a -> Matrix m n2 a -> Matrix m (Add n1 n2) a
appendCol (Matrix x) (Matrix y) = Matrix [xi ++ yi | (xi, yi) <- zip x y]

unconsRow :: Matrix m n a -> Maybe (Vector n a, Matrix (Add m NegOne) n a)
unconsRow (Matrix []) = Nothing
unconsRow (Matrix (x:xs)) = Just (Vector x, Matrix xs)

unconsCol :: Matrix m n a -> Maybe (Vector m a, Matrix m (Add n NegOne) a)
unconsCol (Matrix ([]:_)) = Nothing
unconsCol (Matrix m) = Just (Vector [x | x:_ <- m], Matrix [xs | _:xs <- m])

splitRow :: Matrix m n a -> [Vector n a]
splitRow m = case unconsRow m of
    Nothing -> []
    Just (v, vs) -> v : splitRow vs

splitCol :: Matrix m n a -> [Vector m a]
splitCol m = case unconsCol m of
    Nothing -> []
    Just (v, vs) -> v : splitCol vs

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

qrDecomp :: Matrix m n Float -> (Matrix m m Float, Matrix m n Float) -- m >= n
qrDecomp a = do
    let q = transpose $ Matrix [v | Vector v <- orthonormalize [] (splitCol a)]
    let r = Matrix.mul (transpose q) a
    (q, r)

orthonormalize :: [Vector m Float] -> [Vector m Float] -> [Vector m Float]
orthonormalize e [] = e
orthonormalize e (v:vs) = do
    let v' = normalize $ v - foldr (+) (Vector.mul v 0) [Vector.mul ei (dot ei v) | ei <- e]
    orthonormalize (e ++ [v']) vs

eigenDecomp :: Matrix n n Float -> (Matrix n n Float, Matrix n n Float)
eigenDecomp a = iterate f (a, Matrix.id a) !! 100 where
    f :: (Matrix n n Float, Matrix n n Float) -> (Matrix n n Float, Matrix n n Float)
    f (d, p) = do
        let (q, r) = qrDecomp d
        (Matrix.mul r q, Matrix.mul p q)