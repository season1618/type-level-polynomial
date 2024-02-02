{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector

main :: IO ()
main = do
    let x = Vector [1, 2, 3] :: Vector ('Polynomial ('Cons "a" 'Nil)) Int
    let y = Vector [1, 2, 3] :: Vector ('Polynomial ('Cons "a" 'Nil)) Int
    print $ x == y