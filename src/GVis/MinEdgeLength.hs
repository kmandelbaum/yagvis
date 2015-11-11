{-# LANGUAGE FlexibleContexts #-}
module GVis.MinEdgeLength where

import Numeric.GSL.Minimization
import Data.Graph.Inductive.Graph
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra
import qualified Data.Vector.Generic as GV
import qualified Data.IntSet as IS
import Data.Tuple
import Control.Lens( over, _2)
import Control.Arrow( (&&&) )
import Control.Exception( assert )
import Debug.Trace ( trace )
import GVis.IsotonicRegression (isotonicRegression)
import GVis.GradientProjection (gradientProjection)

dt :: (Show a) => a -> a
dt x = trace (show x) x

spaceWidth :: Double
spaceWidth = 100

nbrCoeff :: Double
nbrCoeff = 100

edgePow :: Double
edgePow = 1.1

nbrPow :: Double
nbrPow = 2

sgnPow :: Floating a => a -> a -> a
sgnPow x = (signum x *) . (abs x**)

(-**-) :: Floating a => a -> a -> a
(-**-) = sgnPow

absPow :: Floating a => a -> a -> a
absPow x = (abs x **)

(|**|) :: Floating a => a -> a -> a
(|**|) = absPow

infixr 8 -**-
infixr 8 |**|

mkCostFunction :: [(Node, Node)] -> IM.IntMap Double -> [(Node, Node)] -> Vector Double -> Double
mkCostFunction levelNbrsRight widths allEdges curX =
  (sum $ map edgeWeight allEdges) + (sum $ map nbrWeight levelNbrsRight)
  where
    edgeWeight (n1, n2) = ((curX ! n1) - (curX ! n2)) |**| edgePow
    nbrWeight (n1, n2) = let d = (curX ! n1 + (widths IM.! n1 + widths IM.! n2) / 2 - curX ! n2)
      in nbrCoeff * if d > 0 then d ** nbrPow else 0

mkCostFunctionGrad :: [(Node, Node)] -> IM.IntMap Double -> [(Node, Node)] -> Vector Double -> Vector Double
mkCostFunctionGrad levelNbrsRight widths allEdges curX =
  GV.map (*2) $
    GV.accum (+) (defaultVector (IM.size widths) 0) $
      assocList ++ map nbrGradAssoc1 levelNbrsRight ++ map nbrGradAssoc2 levelNbrsRight
  where
    edgeGrad (n1, n2) = (n1, edgePow * (curX ! n1 - curX ! n2) -**- (edgePow-1))
    assocList = map edgeGrad allEdges ++ map (edgeGrad . swap) allEdges
    nbrGradAssoc1 (n1,n2) = let d' = nbrCoeff * d n1 n2 in (n1, if d' > 0 then d' else 0)
    nbrGradAssoc2 (n1,n2) = let d' = nbrCoeff * d n1 n2 in (n2, if d' > 0 then negate d' else 0)
    d n1 n2 = (curX ! n1 + (widths IM.! n1 + widths IM.! n2) / 2 - curX ! n2) -**- (nbrPow-1) * nbrPow

-- graph node ids are canonical is asserted
minimizeEdgeLength :: Graph gr => gr a b           -- Graph in a canonical form
                   -> [[Node]]                     -- Nodes at leves
                   -> IM.IntMap Double             -- Node widths
                   -> IM.IntMap Double             -- Node x - coordinates
minimizeEdgeLength g leveling widths = assertContinuousNodes $ toIntMap solution
  where
    solution' = fst $ minimizeVD VectorBFGS 1e-8 10000 0.1 1e-8 costFunction costFunctionGrad startPoint
    solution = last solution''
    --solution = solution'
    solution'' = gradientProjection (mkCostFunction' allEdges)
      (mkProjection leveling startPoint'')
      --id
      startPoint
      1.0 0.7 0.5 1e-3

    costFunction = mkCostFunction levelNbrsRight widths allEdges
    costFunctionGrad = mkCostFunctionGrad levelNbrsRight widths allEdges

    startPoint :: Vector Double
    startPoint = defaultVector (noNodes g) 0 GV.// (concat $ map sumWidth leveling)

    sumWidth xs = zip xs $ zipWith( (+) . (/2) ) ws $ scanl ((+) . (+spaceWidth)) 0 ws
      where ws = map (widths IM.!) xs

    startPoint'' :: Vector Double
    startPoint'' = defaultVector (noNodes g) 0 GV.// (concat $ map sumWidth'' leveling)

    sumWidth'' xs = zip xs $ zipWith( (+) . (/2) ) ws $ scanl (+) 0 ws
      where ws = map (widths IM.!) xs
      

    toIntMap :: Vector Double -> IM.IntMap Double
    toIntMap = IM.fromList . zip [0..] . GV.toList

    ns = [0..noNodes g - 1]
    allEdges = edges g
    levelNbrsRight = concatMap ( \xs -> zip xs (tail xs) ) leveling

    assertContinuousNodes = assert $ IS.fromList (nodes g) == IS.fromList ns

defaultVector :: GV.Vector v a => Int -> a -> v a
defaultVector = GV.replicate

-- the target function is represented as norm(A x, alpha) ** alpha
-- A is a sparse matrix of edges

mkMatrix :: [(Node, Node)] -> GMatrix
mkMatrix allEdges = mkSparse $ foldr (\(i, e) m -> ((i, fst e), -1):((i, snd e), 1):m) [] (zip [1..] allEdges)

mkCostFunction' :: [(Node, Node)] ->  Vector Double -> (Double, Vector Double)
mkCostFunction' e v =
  (GV.sum fv, tr mat !#> gv)
  where mv = mat !#> v
        mat = mkMatrix e
        --gv = cmap ( (*edgePow) . (-**- (edgePow - 1))) mv
        --fv = cmap (|**| edgePow) mv
        fv = cmap el_f mv
        gv = cmap el_g mv
        el_f x = if abs x < d then 0.5 * (d ** (edgePow - 2)) * x ** 2
          else (x |**| edgePow) - 0.5 * d ** edgePow
        el_g x = if abs x < d then  (d ** (edgePow - 2)) * x
          else edgePow * (x -**- (edgePow - 1))
        d = 5.0

mkProjection :: [[Node]] -> Vector Double -> Vector Double -> Vector Double
mkProjection leveling x0 x = GV.accum (+) x0 $
  concat $
  (zipWith zip) leveling $
  map (isotonicRegression . map ((x - x0) GV.!)) leveling
