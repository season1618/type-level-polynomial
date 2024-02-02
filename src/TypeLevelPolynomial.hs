{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevelPolynomial (
    List(Nil, Cons),
    Polynomial(Polynomial),
) where

import GHC.TypeLits

data List a = Nil | Cons a (List a)

type family Merge (x :: List a) (y :: List a) :: List a where
    Merge Nil y = y
    Merge x Nil = x
    Merge (Cons x xs) (Cons y ys) = MergeIf (CmpSymbol x y) (Cons x (Merge xs (Cons y ys))) (Cons y (Merge (Cons x xs) ys))

type family MergeIf (c :: Ordering) (t :: List a) (e :: List a) :: List a where
    MergeIf EQ t e = t
    MergeIf LT t e = t
    MergeIf GT t e = e

data Polynomial = Polynomial (List Symbol)

type family Add (x :: Polynomial) (y :: Polynomial) :: Polynomial where
    Add ('Polynomial x) ('Polynomial y) = 'Polynomial (Merge x y)