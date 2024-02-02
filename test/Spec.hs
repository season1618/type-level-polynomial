{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector

main :: IO ()
main = do
    let x = Vector [1, 2, 3] :: Vector ('Polynomial ('Cons "a" 'Nil)) Float
    let y = Vector [2, 3, 4] :: Vector ('Polynomial ('Cons "b" 'Nil)) Float
    print $ append x y == append y x
    print $ x + x
    print $ x - x
    print $ x `dot` x
    print $ norm x