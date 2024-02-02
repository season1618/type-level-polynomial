module TypeLevelPolynomial (
    Polynomial(Polynomial),
) where

import GHC.TypeLits

data Polynomial = Polynomial [Symbol]