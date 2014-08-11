{-# LANGUAGE TypeFamilies,TypeSynonymInstances #-}
module GVis.GVis where 

import Data.Graph.Inductive hiding ( (&) )
import Data.Graph.Inductive.Basic
--import Data.Graph.Inductive.Graph
import Diagrams.Prelude
import Data.GraphViz (dotToGraph, Attributes)
import Data.Maybe

hvcat' ::  ( Monoid a, 
             HasOrigin a,
             Juxtaposable a,
             Semigroup a,
             V a ~ R2 ) =>
    CatOpts R2 -> CatOpts R2 -> [[a]] -> a
hvcat' hCatOpts vCatOpts xs = vcat' vCatOpts $ map (hcat' hCatOpts) xs

class Visual v where
  toDiagram :: v -> Diagram c R2 

graphNodeToDiagram :: (Graph gr, Visual a) => 
    Node -> gr a b -> Diagram c R2
graphNodeToDiagram n g = toDiagram ( fromJust $ lab g n ) # named n
