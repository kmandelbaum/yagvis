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


-- visualize with levels
-- the graph should be acyclic
--
visualizeWithLevels :: ( Graph gr,
                         Renderable DiaText.Text c, 
                         Renderable (Path R2) c ) => 
    gr (Diagram c R2) b -> Diagram c R2
visualizeWithLevels g = hvcat' (with & sep .~ 10.0 ) (with & sep .~ 10.0) levels
    where levels =  map ( map getNode . snd) $ assocs $ toLevels g
          getNode = fromJust . (lab  g)
    

visDefault :: (DynGraph gr, Renderable DiaText.Text c, Renderable (Path R2) c) => gr a b -> gr (Diagram c R2) b
visDefault = gmap (\(pre, n, a, suc) -> (pre, n, ellipseWithName (show n), suc) ) 
