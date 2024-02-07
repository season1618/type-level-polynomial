{-# LANGUAGE DataKinds #-}

import TypeLevelPolynomial
import Vector as Vec
import Matrix as Mat
import Test.HUnit

type A = 'Polynomial '[ '("a", 'Pos 1) ]
type B = 'Polynomial '[ '("b", 'Pos 1) ]
type N = 'Polynomial '[ '("n", 'Pos 1) ]

main :: IO ()
main = do
    let v1 = Vector [1, 2, 3] :: Vector ('Polynomial '[ '("1", 'Pos 3) ]) Float
    let v2 = Vector [4, 5] :: Vector ('Polynomial '[ '("1", 'Pos 2) ]) Float
    let v3 = Vector [1, 2, 3, 4, 5] :: Vector ('Polynomial '[ '("1", 'Pos 5)]) Float
    let v4 = Vector [1, 2, 3] :: Vector A Float
    let v5 = Vector [2, 3, 4] :: Vector B Float

    let vectorTest = TestList [ TestCase $ assertEqual "append" (append v1 v2) v3
                              , TestCase $ assertEqual "uncons, scalar" (let Just (x, xs) = Vec.uncons v4 in append (scalar x) xs) v4
                              , TestCase $ assertEqual "sum, product" (v3 + v3 + v3) (Vec.mul v3 3)
                              , TestCase $ assertEqual "dot" (dot v4 v4) 14
                              ]
    runTestTT vectorTest

    let m1 = Matrix [[1, 2, 3], [2, 3, 4]] :: Matrix A B Float
    let m2 = transpose m1
    let m3 = Matrix [ [8, 16, 24, 32], [2,  7, 12, 17], [6, 17, 32, 59], [7, 22, 46, 105] ] :: Matrix N N Float

    let matrixTest = TestList [ TestCase $ assertEqual "append-row" (appendRow m1 m1) (Matrix [[1, 2, 3], [2, 3, 4], [1, 2, 3], [2, 3, 4]])
                              , TestCase $ assertEqual "append-col" (appendCol m1 m1) (Matrix [[1, 2, 3, 1, 2, 3], [2, 3, 4, 2, 3, 4]])
                              , TestCase $ assertEqual "add" (m1 + m1) (Matrix [[2, 4, 6], [4, 6, 8]])
                              , TestCase $ assertEqual "mul" (Mat.mul m1 m2) (Matrix [[14, 20], [20, 29]])
                              , TestCase $ assertEqual "mul" (Mat.mul m2 m1) (Matrix [[5, 8, 11], [8, 13, 18], [11, 18, 25]])
                              , TestCase $ assertEqual "LU decomposition" (let (l, u) = luDecomp m3 in Mat.mul l u) m3
                              , TestCase $ assertEqual "QR decomposition" (let (q, r) = qrDecomp m3 in Mat.mul q r) m3
                              , TestCase $ assertEqual "eigen decomposition" (let (d, p) = eigenDecomp m3 in Mat.mul (Mat.mul p d) (transpose p)) m3
                              ]
    runTestTT matrixTest

    let (l, u) = luDecomp m3
    putStrLn $ "L = " ++ show l
    putStrLn $ "U = " ++ show u

    let (q, r) = qrDecomp m3
    putStrLn $ "Q = " ++ show q
    putStrLn $ "R = " ++ show r

    let (d, p) = eigenDecomp m3
    putStrLn $ "D = " ++ show d
    putStrLn $ "P = " ++ show p