{-# LANGUAGE TypeFamilies,TypeSynonymInstances,RankNTypes,FlexibleContexts #-}
module GVis.GVis where

import Data.Graph.Inductive hiding ( (&), Path )
--import Data.Graph.Inductive.Graph
import Diagrams.Prelude
import Control.Lens( _1, traverse, over )
import GVis.GraphAlgo
import Data.Array

hvcat' ::  ( Monoid a,
             HasOrigin a,
             Juxtaposable a,
             Semigroup a,
             Alignable a,
             V a ~ R2 ) =>
    CatOpts R2 -> CatOpts R2 -> [[a]] -> a
hvcat' hCatOpts vCatOpts xs = vcat' vCatOpts $ map ( \x -> hcat' hCatOpts x # center ) xs

--internal representation data type for visualizable graphs
--
--

data Shape = Rectangle | Ellipse

data GVRepNode a = GVRepNode { isVisible :: Bool,
                               nodeData :: Maybe a,
                               gvLevel :: Int }

data GVRepEdge a = GVRepEdge { isPartial :: Bool,
                               edgeData :: Maybe a }

type GVRepGraph gr a b = DynGraph gr => gr (GVRepNode a) (GVRepEdge b)

convertGraph :: forall a b gr . DynGraph gr => gr a b -> GVRepGraph gr a b
convertGraph g = insertPseudo $ gmap ( \(p, n, l, s) -> ( bindEdges p, n, bindVertex l n, bindEdges s ) ) g
  where
    bindVertex x n = GVRepNode{ isVisible = True, nodeData = Just x, gvLevel = undefined }
    bindEdge x = GVRepEdge{ isPartial = False, edgeData = Just x }
    bindEdges = over (traverse . _1) bindEdge
    allEdges = labEdges g
    insertPseudo :: GVRepGraph gr a b -> GVRepGraph gr a b
    insertPseudo g' = foldr (undefined) g' $ filter isCrossLevel allEdges
    isCrossLevel :: LEdge b -> Bool
    isCrossLevel = undefined
