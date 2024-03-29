{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Matrix where

import TypeLevelPolynomial
import Data.Vector as Vec

data Matrix (m :: Polynomial) (n :: Polynomial) a = Matrix [[a]]
    deriving Eq

instance Show a => Show (Matrix m n a) where
    show (Matrix m) = "(\n" ++ foldr (++) "" (map showList m) ++ ")" where
        showList :: Show a => [a] -> String
        showList [] = "\n"
        showList [a] = show a ++ "\n"
        showList (a:as) = show a ++ ", " ++ showList as

ident :: Matrix n n Float -> Matrix n n Float
ident (Matrix [[_]]) = Matrix [[1]]
ident a = do
    let Just (_, a0j, ai0, aij) = decomp a
    comp 1 (zero a0j) (zero ai0) (ident aij)

rowVector :: Vector n a -> Matrix One n a
rowVector (Vector v) = Matrix [v]

colVector :: Vector n a -> Matrix n One a
colVector (Vector v) = Matrix [[vi] | vi <- v]

appendRow :: Matrix m1 n a -> Matrix m2 n a -> Matrix (Add m1 m2) n a
appendRow (Matrix x) (Matrix y) = Matrix (x ++ y)

appendCol :: Matrix m n1 a -> Matrix m n2 a -> Matrix m (Add n1 n2) a
appendCol (Matrix x) (Matrix y) = Matrix [xi ++ yi | (xi, yi) <- zip x y]

consRow :: Vector n a -> Matrix ('Prev m) n a -> Matrix m n a
consRow (Vector v) (Matrix m) = Matrix (v:m)

consCol :: Vector m a -> Matrix m ('Prev n) a -> Matrix m n a
consCol (Vector v) (Matrix m) = Matrix [vi:mi | (vi, mi) <- zip v m]

unconsRow :: Matrix m n a -> Maybe (Vector n a, Matrix ('Prev m) n a)
unconsRow (Matrix []) = Nothing
unconsRow (Matrix (x:xs)) = Just (Vector x, Matrix xs)

unconsCol :: Matrix m n a -> Maybe (Vector m a, Matrix m ('Prev n) a)
unconsCol (Matrix ([]:_)) = Nothing
unconsCol (Matrix m) = Just (Vector [x | x:_ <- m], Matrix [xs | _:xs <- m])

concatRow :: [Vector n a] -> Matrix m n a
concatRow [v] =
    let Matrix m = rowVector v
    in Matrix m
concatRow (v:vs) =
    let Matrix m = appendRow (rowVector v) (concatRow vs)
    in Matrix m

concatCol :: [Vector m a] -> Matrix m n a
concatCol [v] =
    let Matrix m = colVector v
    in Matrix m
concatCol (v:vs) =
    let Matrix m = appendCol (colVector v) (concatCol vs)
    in Matrix m

splitRow :: Matrix m n a -> [Vector n a]
splitRow m = case unconsRow m of
    Nothing -> []
    Just (v, vs) -> v : splitRow vs

splitCol :: Matrix m n a -> [Vector m a]
splitCol m = case unconsCol m of
    Nothing -> []
    Just (v, vs) -> v : splitCol vs

transpose :: Matrix m n a -> Matrix n m a
transpose a = case unconsRow a of
    Nothing -> Matrix (repeat [])
    Just (v, m) -> consCol v (transpose m)

inverse :: Matrix n n Float -> Matrix n n Float
inverse m = do
    let (l, u) = luDecomp m
        i = ident m
    concatCol [inverseU u (inverseL l b) | b <- splitCol i] where
        inverseL :: Matrix n n Float -> Vector n Float -> Vector n Float -- Lx = b
        inverseL (Matrix [[l00]]) (Vector [v0]) = Vector [v0 / l00]
        inverseL l b = do
            let Just (l00, _, li0, lij) = decomp l
                Just (b0, bi) = uncons b
            cons (b0 / l00) (inverseL lij (bi - li0 `Vec.mul` (b0 / l00)))
        inverseU :: Matrix n n Float -> Vector n Float -> Vector n Float -- Ux = b
        inverseU (Matrix [[_]]) (Vector [v0]) = Vector [v0]
        inverseU u b = do
            let Just (_, u0j, _, uij) = decomp u
                Just (b0, bi) = uncons b
            let xi = inverseU uij bi
            cons (b0 - dot u0j xi) xi

instance Num a => Num (Matrix m n a) where
    (+) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (+)) x y)
    (-) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (-)) x y)

mul :: Num a => Matrix l m a -> Matrix m n a -> Matrix l n a
mul (Matrix x) (Matrix y) = Matrix [mulVecMat xi y | xi <- x] where
    mulVecMat :: Num a => [a] -> [[a]] -> [a]
    mulVecMat xi y = sumVec [[xik * ykj | ykj <- yk] | (xik, yk) <- zip xi y] where
        sumVec :: Num a => [[a]] -> [a]
        sumVec = foldr (zipWith (+)) (repeat (fromInteger 0))

decomp :: Matrix m n a -> Maybe (a, Vector ('Prev n) a, Vector ('Prev m) a, Matrix ('Prev m) ('Prev n) a)
decomp m = do
    let Just (mi0', mij') = unconsRow m
        Just (m00, m0j) = uncons mi0'
        Just (mi0, mij) = unconsCol mij'
    Just (m00, m0j, mi0, mij)

comp :: a -> Vector ('Prev n) a -> Vector ('Prev m) a -> Matrix ('Prev m) ('Prev n) a -> Matrix m n a
comp a00 a0j ai0 aij = consRow (cons a00 a0j) (consCol ai0 aij)

luDecomp :: Matrix n n Float -> (Matrix n n Float, Matrix n n Float)
luDecomp (Matrix [[a]]) = (Matrix [[a]], Matrix [[1]])
luDecomp a = do
    let Just (a00, a0j, ai0, aij) = decomp a
    let l00 = a00
        l0j = zero a0j 
        li0 = ai0
    let u00 = 1
        u0j = Vec.div a0j a00
        ui0 = zero ai0
    let (lij, uij) = luDecomp (aij - Data.Matrix.mul (colVector li0) (rowVector u0j))
    let l = comp l00 l0j li0 lij
        u = comp u00 u0j ui0 uij
    (l, u)

qrDecomp :: Matrix m n Float -> (Matrix m m Float, Matrix m n Float) -- m >= n
qrDecomp a = do
    let q = transpose $ Matrix [v | Vector v <- orthonormalize [] (splitCol a)]
    let r = Data.Matrix.mul (transpose q) a
    (q, r)

eigenDecomp :: Matrix n n Float -> (Matrix n n Float, Matrix n n Float)
eigenDecomp a = iterate f (a, ident a) !! 100 where
    f :: (Matrix n n Float, Matrix n n Float) -> (Matrix n n Float, Matrix n n Float)
    f (d, p) = do
        let (q, r) = qrDecomp d
        (Data.Matrix.mul r q, Data.Matrix.mul p q)