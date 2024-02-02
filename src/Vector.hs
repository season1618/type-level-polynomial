{-# LANGUAGE KindSignatures #-}

module Vector where

import TypeLevelPolynomial

data Vector (n :: Polynomial) a = Vector [a]
    deriving (Eq, Show)