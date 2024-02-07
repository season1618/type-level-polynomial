{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module ML.LinearRegression where

import Data.Vector
import Data.Matrix as Mat

data LinearReg n_input = LinearReg (Vector n_input Float)
    deriving Show

train :: [(Vector n_input Float, Float)] -> LinearReg n_input
train trainData = do
    let (xs', ys') = unzip trainData
    let xs = concatRow xs'
        ys = Vector ys'
    let w:_ = splitCol $ inverse (Mat.mul (transpose xs) xs) `Mat.mul` (transpose xs) `Mat.mul` (colVector ys)
    LinearReg w

predict :: LinearReg n_input -> Vector n_input Float -> Float
predict (LinearReg model) input = dot model input