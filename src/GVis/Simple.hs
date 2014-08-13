{-# LANGUAGE FlexibleContexts #-}
module GVis.Simple where

import GVis.GVis
import Diagrams.Prelude
import Data.Graph.Inductive hiding (Path, (&))
import GVis.GraphAlgo
import Diagrams.TwoD.Text as DiaText
import Data.Array
import Data.Maybe

ellipseWithName n = ellipseXY 200 100 <> ( text n # fontSize (Local 40) )
rectangleWithName n = rect 200 100 <> ( text n # fontSize (Local 40) )

defaultSpacing :: Double
defaultSpacing = 100.0

defaultArrowOpt = with & gaps .~ small

-- visualize with levels
-- the graph should be acyclic
--
visualizeWithLevels :: ( Graph gr,
                         Renderable DiaText.Text c, 
                         Renderable (Path R2) c ) => 
    gr (Diagram c R2) b -> Diagram c R2
visualizeWithLevels g = hvcat' ( with & sep .~ defaultSpacing ) ( with & sep .~ defaultSpacing ) levels
    where levels = map ( map ( \x -> getNode x # named x ) . snd ) $ assocs $ toLevels g
          getNode = fromJust . lab g

vertToEll :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) => gr a b -> gr (Diagram c R2) b
vertToEll = gmap ( \(pre, n, a, suc) -> (pre, n, ellipseWithName (show n), suc) ) 

vertToRect :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) => gr a b -> gr (Diagram c R2) b
vertToRect = gmap ( \(pre, n, a, suc) -> (pre, n, rectangleWithName (show n), suc) ) 

edgeSimple :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) =>
    gr (Diagram c R2) b -> Diagram c R2

edgeSimple g = connectEdges g $ visualizeWithLevels g
    where connectEdges g = foldr ( (.) . uncurry ( connectOutside' defaultArrowOpt ) ) id $ edges g
