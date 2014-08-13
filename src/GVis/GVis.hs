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
             Alignable a,
             V a ~ R2 ) =>
    CatOpts R2 -> CatOpts R2 -> [[a]] -> a
hvcat' hCatOpts vCatOpts xs = vcat' vCatOpts $ map ( \x -> hcat' hCatOpts x # center ) xs

