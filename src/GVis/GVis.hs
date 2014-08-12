{-# LANGUAGE TypeFamilies,TypeSynonymInstances,RankNTypes,FlexibleContexts #-}
module GVis.GVis where 

import Data.Graph.Inductive hiding ( (&), Path )
import Data.Graph.Inductive.Basic
--import Data.Graph.Inductive.Graph
import Diagrams.Prelude
import Data.GraphViz (dotToGraph, Attributes)
import Data.Maybe
import Diagrams.TwoD.Text as DiaText

hvcat' ::  ( Monoid a, 
             HasOrigin a,
             Juxtaposable a,
             Semigroup a,
             V a ~ R2 ) =>
    CatOpts R2 -> CatOpts R2 -> [[a]] -> a
hvcat' hCatOpts vCatOpts xs = vcat' vCatOpts $ map (hcat' hCatOpts) xs

-- class for type representable as 2-D diagrams
-- backend should render text and paths
class Visual v where
    toDiagram :: forall c . ( Renderable (Path R2) c,
                              Renderable DiaText.Text c ) => 
                v -> Diagram c R2 

graphNodeToDiagram :: (Graph gr, Visual a, Renderable (Path R2) c, Renderable Text c ) => 
    Node -> gr a b -> Diagram c R2
graphNodeToDiagram n g = toDiagram ( fromJust $ lab g n ) # named n
