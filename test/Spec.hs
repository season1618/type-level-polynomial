{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector
import Matrix

main :: IO ()
main = do
    let x = Vector [1, 2, 3] :: Vector ('Polynomial ('Cons "a" 'Nil)) Float
    let y = Vector [2, 3, 4] :: Vector ('Polynomial ('Cons "b" 'Nil)) Float
    print $ append x y == append y x
    print $ x + x
    print $ x - x
    print $ x `dot` x
    print $ norm x

    let a = Matrix [[1, 2, 3], [2, 3, 4]] :: Matrix ('Polynomial ('Cons "a" 'Nil)) ('Polynomial ('Cons "b" 'Nil)) Float
    let b = Matrix [[1, 2], [2, 3], [3, 4]] :: Matrix ('Polynomial ('Cons "b" 'Nil)) ('Polynomial ('Cons "a" 'Nil)) Float
    print $ a + a
    print $ b + b
    print $ mul a b
    print $ mul b a