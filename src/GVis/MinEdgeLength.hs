{-# LANGUAGE FlexibleContexts #-}
module GVis.MinEdgeLength where

import Numeric.GSL.Minimization
import Data.Graph.Inductive.Graph
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra.Data
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.IntSet as IS
import Data.Tuple
import Control.Lens( over, _2)
import Control.Arrow( (&&&) )

import Control.Exception( assert )

import Debug.Trace ( trace )

dt :: (Show a) => a -> a
dt x = trace (show x) x

spaceWidth :: Double
spaceWidth = 100.0

sgnPow :: Floating a => a -> a -> a
sgnPow x = (signum x *) . (abs x**)

-- graph node ids are canonical is asserted
minimizeEdgeLength :: Graph gr => gr a b           -- Graph in a canonical form
                   -> [[Node]]                     -- Nodes at leves
                   -> IM.IntMap Double             -- Node widths
                   -> IM.IntMap Double             -- Node x - coordinates
minimizeEdgeLength g leveling widths = assertContinuousNodes $ toIntMap solution
  where
    solution = fst $ minimizeVD ConjugatePR 1e-6 2000 1 1e-6 costFunction costFunctionGrad startPoint

    costFunction :: Vector Double -> Double
    costFunction curX = (sum $ map edgeWeight allEdges) + (sum $ map (uncurry nbrWeight) levelNbrsRight)
      where
        --edgeWeight (n1, n2) = ((curX ! n1) - (curX ! n2)) ** 2
        edgeWeight (n1, n2) = abs ((curX ! n1) - (curX ! n2)) ** 1.2 
        nbrWeight n1 n2 = 10 * if d > 0 then d ** 2 else 0
          where 
            d = (curX ! n1 + (widths IM.! n1 + widths IM.! n2) / 2 - curX ! n2)

    costFunctionGrad :: Vector Double -> Vector Double
    costFunctionGrad curX = GV.map (*2) $ GV.accum (+) (defaultVector 0) $
        assocList ++ map nbrGradAssoc1 levelNbrsRight ++ map nbrGradAssoc2 levelNbrsRight
      where
        edgeGrad (n1, n2) = (n1, 0.6 * sgnPow (curX ! n1 - curX ! n2) 0.2)
        assocList = map edgeGrad allEdges ++ map (edgeGrad . swap) allEdges
        nbrGradAssoc1 (n1,n2) = let d' = 10 * d n1 n2 in (n1, if d' > 0 then d' else 0)
        nbrGradAssoc2 (n1,n2) = let d' = 10 * d n1 n2 in (n2, if d' > 0 then negate d' else 0)
        d n1 n2 = (curX ! n1 + (widths IM.! n1 + widths IM.! n2) / 2 - curX ! n2)

    startPoint :: Vector Double
    startPoint = defaultVector 0 GV.// (concat $ map sumWidth leveling)

    sumWidth xs = zip xs $ zipWith( (+) . (/2) ) ws $ scanl ((+) . (+spaceWidth)) 0 ws
      where ws = map (widths IM.!) xs

    toIntMap :: Vector Double -> IM.IntMap Double
    toIntMap = IM.fromList . zip [0..] . GV.toList

    ns = [0..noNodes g - 1]
    allEdges = edges g

    levelNbrsRight = concatMap ( \xs -> zip xs (tail xs) ) leveling

    defaultVector :: GV.Vector v a => a -> v a
    defaultVector = GV.replicate (noNodes g)

    assertContinuousNodes = assert $ IS.fromList (nodes g) == IS.fromList ns
