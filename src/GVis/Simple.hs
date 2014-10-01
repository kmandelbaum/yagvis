{-# LANGUAGE FlexibleContexts #-}
module GVis.Simple where

import GVis.GVis
import Diagrams.Prelude
import Data.Graph.Inductive hiding (Path, (&))
import GVis.GraphAlgo
import Diagrams.TwoD.Text as DiaText
import Data.Maybe
import Data.IntMap( assocs )

ellipseWithName n = ellipseXY 200 100 <> ( text n # fontSize (Local 40) )
rectangleWithName n = rect 200 100 <> ( text n # fontSize (Local 40) )

defaultSpacing :: Double
defaultSpacing = 100.0

-- visualize with levels
-- the graph should be acyclic
--
visualizeWithLevels :: ( Graph gr,
                         Renderable DiaText.Text c, 
                         Renderable (Path R2) c ) => 
    gr (Diagram c R2) b -> Diagram c R2
visualizeWithLevels g = hvcat' ( with & sep .~ defaultSpacing ) ( with & sep .~ defaultSpacing ) levels
    where levels = map ( map ( \x -> fromJust (lab g x) # named x ) . snd ) $ assocs $ toLevels g

vertToEll :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) => gr a b -> gr (Diagram c R2) b
vertToEll = gmap ( \(p, n, _, s) -> (p, n, ellipseWithName (show n), s) ) 

vertToRect :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) => gr a b -> gr (Diagram c R2) b
vertToRect = gmap ( \(p, n, _, s) -> (p, n, rectangleWithName (show n), s) ) 

edgeSimple :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) =>
    gr (Diagram c R2) b -> Diagram c R2

edgeSimple g = connectEdges $ visualizeWithLevels g
    where connectEdges g' = foldr ( uncurry ( connectOutside' defaultArrowOpt ) ) g' $ edges g
