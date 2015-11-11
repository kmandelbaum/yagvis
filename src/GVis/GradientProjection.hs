module GVis.GradientProjection where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra
import Debug.Trace

n2 x = dot x x

alphaMin = 0.0001
alphaMax = 100000

gradientProjection :: (Vector Double -> (Double, Vector Double)) ->
  (Vector Double -> Vector Double) ->
  Vector Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  [Vector Double]
gradientProjection fun project x0 alpha0 sigma gamma eps = go x0 alpha0
  where
    go :: Vector Double -> Double -> [Vector Double]
    go x alpha = if norm_2 d < eps then [x] else x:go newX newAlpha
      where
        (f,g) = fun x
        d = project (x - scalar alpha * g) - x
        newX = lineScan 1
        b = ((snd (fun newX) - g) <.> (newX - x))
        newAlpha = if b <= 0 then alphaMax
          else
            min alphaMax $ max alphaMin $ n2 (newX - x) / b
        lineScan lam
          | (fst $ fun (x + d * scalar lam)) < f + gamma * lam * (g <.> d) =
              x + d * scalar lam
          | otherwise = lineScan (lam * sigma)
