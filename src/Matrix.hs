{-# LANGUAGE KindSignatures #-}

module Matrix where

import TypeLevelPolynomial

data Matrix (n :: Polynomial) (m :: Polynomial) a = Matrix [[a]]
    deriving (Eq, Show)

instance Num a => Num (Matrix n m a) where
    (+) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (+)) x y)
    (-) (Matrix x) (Matrix y) = Matrix (zipWith (zipWith (-)) x y)