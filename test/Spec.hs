{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector
import Matrix

main :: IO ()
main = do
    let x = Vector [1, 2, 3] :: Vector ('Polynomial '[ '("1", 'Pos 3) ]) Float
    let y = Vector [4, 5] :: Vector ('Polynomial '[ '("1", 'Pos 2) ]) Float
    let z = Vector [1, 2, 3, 4, 5] :: Vector ('Polynomial '[ '("1", 'Pos 5)]) Float
    print $ append x y == z

    let x = Vector [1, 2, 3] :: Vector ('Polynomial '[ '("a", 'Pos 1) ]) Float
    let y = Vector [2, 3, 4] :: Vector ('Polynomial '[ '("b", 'Pos 1) ]) Float
    print $ append x y == append y x
    print $ x + x
    print $ x - x
    print $ x `dot` x
    print $ norm x

    let a = Matrix [[1, 2, 3], [2, 3, 4]] :: Matrix ('Polynomial '[ '("a", 'Pos 1) ]) ('Polynomial '[ '("b", 'Pos 1) ]) Float
    let b = Matrix [[1, 2], [2, 3], [3, 4]] :: Matrix ('Polynomial '[ '("b", 'Pos 1) ]) ('Polynomial '[ '("a", 'Pos 1) ]) Float
    print $ appendRow a a
    print $ appendCol a a
    print $ a + a
    print $ b + b
    print $ mul a b
    print $ mul b a