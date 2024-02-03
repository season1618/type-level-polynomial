{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevelPolynomial (
    Polynomial(Polynomial),
    Add,
) where

import GHC.TypeLits

type family Merge (x :: [a]) (y :: [a]) :: [a] where
    Merge '[] y = y
    Merge x '[] = x
    Merge (x:xs) (y:ys) = MergeIf (CmpSymbol x y) (x : (Merge xs (y:ys))) (y : (Merge (x:xs) ys))

type family MergeIf (c :: Ordering) (t :: [a]) (e :: [a]) :: [a] where
    MergeIf EQ t e = t
    MergeIf LT t e = t
    MergeIf GT t e = e

data Polynomial = Polynomial [Symbol]

type family Add (x :: Polynomial) (y :: Polynomial) :: Polynomial where
    Add ('Polynomial x) ('Polynomial y) = 'Polynomial (Merge x y)