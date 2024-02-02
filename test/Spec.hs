{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector

main :: IO ()
main = do
    let x = Vector [1, 2, 3] :: Vector ('Polynomial ["0", "a"]) Int
    let y = Vector [1, 2, 3] :: Vector ('Polynomial ["0", "a"]) Int
    print $ x == y