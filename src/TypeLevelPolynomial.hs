{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevelPolynomial (
    Zahl(Pos, Zero, Neg),
    Polynomial(Polynomial),
    Add,
) where

import GHC.TypeLits

data Zahl = Pos Nat
          | Zero
          | Neg Nat

type family AddZahl (x :: Zahl) (y :: Zahl) :: Zahl where
    AddZahl x 'Zero = x
    AddZahl 'Zero x = x
    AddZahl ('Pos x) ('Pos y) = 'Pos (x + y)
    AddZahl ('Pos x) ('Neg y) = CmpAddZahl (CmpNat x y) x y
    AddZahl ('Neg x) ('Pos y) = CmpAddZahl (CmpNat y x) y x
    AddZahl ('Neg x) ('Neg y) = 'Neg (x + y)

type family CmpAddZahl (c :: Ordering) (x :: Nat) (y :: Nat) :: Zahl where
    CmpAddZahl 'LT x y = 'Neg (y - x)
    CmpAddZahl 'EQ x y = 'Zero
    CmpAddZahl 'GT x y = 'Pos (x - y)

data Polynomial = Polynomial [(Symbol, Zahl)]

type family Add (x :: Polynomial) (y :: Polynomial) :: Polynomial where
    Add ('Polynomial x) ('Polynomial y) = 'Polynomial (AddList x y)

type family AddList (x :: [(Symbol, Zahl)]) (y :: [(Symbol, Zahl)]) :: [(Symbol, Zahl)] where
    AddList '[] y = y
    AddList x '[] = x
    AddList ('(v1, k1) : xs) ('(v2, k2) : ys) = CmpAddList (CmpSymbol v1 v2) ('(v1, k1) : xs) ('(v2, k2) : ys)

type family CmpAddList (c :: Ordering) (x :: [(Symbol, Zahl)]) (y :: [(Symbol, Zahl)]) :: [(Symbol, Zahl)] where
    CmpAddList 'LT (x:xs) (y:ys) = x : AddList xs (y:ys)
    CmpAddList 'EQ ('(v1, k1) : xs) ('(v2, k2) : ys) = '(v1, AddZahl k1 k2) : AddList xs ys
    CmpAddList 'GT (x:xs) (y:ys) = y : AddList (x:xs) ys