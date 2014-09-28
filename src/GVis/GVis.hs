{-# LANGUAGE TypeFamilies,TypeSynonymInstances,RankNTypes,FlexibleContexts,ScopedTypeVariables #-}
module GVis.GVis where

import Data.Graph.Inductive hiding ( (&), Path )
--import Data.Graph.Inductive.Graph
import Diagrams.Prelude
import Control.Lens( _1, traverse, over )
import GVis.GraphAlgo
import Data.Array ( (!) )
import Data.Maybe
import Diagrams.TwoD.Text as DiaText
import Data.Default
import Control.Arrow( (***) )
import Data.IntMap( fromListWith, IntMap, elems )
import Data.Tuple( swap )
import Data.Monoid.Reducer( unit )
import Data.Monoid ( mappend )

import Debug.Trace as DT

class (Renderable DiaText.Text c, Renderable (Path R2) c) => TwoDRender c

type TwoDDiagram c = Diagram c R2

catopts  = ( with & sep .~ 100.0 )

defaultArrowOpt :: ArrowOpts
defaultArrowOpt = with & gaps .~ small

-- horizontal into vertical cat
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

data Shape = Rectangle | Ellipse deriving Show

-- all visible nodes contain nodeData
-- nodeData = Nothing means that node is invisible
data GVRepNode a = GVRepNode { gvNodeData :: Maybe a, gvLevel :: Int }
  deriving Show

--  gvEdgeData = Just x (x is visualization data ) - visible
--  gvEdgeData = Nothing - invisible
--  gvPartNodes - [] for short edges (x:xs) for long edges

data GVRepEdge a = GVRepEdge { gvEdgeData :: Maybe a, gvPartNodes :: [Node] }
  deriving Show

type GVRepGraph gr a b = DynGraph gr => gr (GVRepNode a) (GVRepEdge b)

convertGraph :: forall a b gr . DynGraph gr => gr a b -> GVRepGraph gr a b
convertGraph g = ( bindCross . bindRegular . removeCrossEdges ) g
  where
    bindVertex x n = GVRepNode{ gvNodeData = Just x, gvLevel = levels ! n }
    bindEdge x = GVRepEdge{ gvEdgeData = Just x, gvPartNodes = [] }
    bindEdges = over (traverse . _1) bindEdge

    bindRegular = gmap ( \(p, n, l, s) -> ( bindEdges p, n, bindVertex l n, bindEdges s ) )
    bindCross g'' = foldr splitEdge g'' crossEdges

    splitEdge e@(from, to, d) g'' = insEdge (from, to, oldEdge) $ insEdges ne $ insNodes nodeList g''
      where nodeList = zip nn $ map mkInvNode newLevels
            mkInvNode l = GVRepNode{ gvNodeData = Nothing, gvLevel = l }
            nn = newNodes q g''
            ne = zipWith (\f t -> (f, t, dummyEdge)) (from:nn) (nn ++ [to])
            dummyEdge = GVRepEdge{ gvEdgeData = Nothing, gvPartNodes = [] }
            q = levelDelta e - 1
            newLevels = [ levels ! from + 1 .. levels ! to - 1]
            oldEdge = GVRepEdge{ gvEdgeData = Just d, gvPartNodes = nn }

    levels = earlyTimes g
    levelDelta (from, to, _) = levels ! to - levels ! from
    isCrossLevel e = levelDelta e > 1

    crossEdges = filter isCrossLevel $ labEdges g
    removeCrossEdges = efilter ( not . isCrossLevel )

graphToDia :: (DynGraph gr, TwoDRender c) => GVRepGraph gr a b -> TwoDDiagram c
graphToDia g = connectShort g $ hvcat' catopts catopts dLevels
  where dLevels = ( map . map ) nodeToDia $ elems nLevels

        nLevels = fromListWith mappend acsList
          where acsList = map ( (gvLevel *** unit) . swap ) $ labNodes g

        nodeToDia n = named n $ maybe invisNode (const visNode) d
          where GVRepNode{ gvNodeData = d, gvLevel = l } = fromJust $ lab g n
                invisNode = rect 100 100 # lw none
                visNode = ellipseXY 200 100 # lw thick <> text (show n) # fontSize (Local 40)

connectShort g d = straitArrows d $ map (\(x,y,_) -> (x,y) ) $ filter isShort $ labEdges g
  where isShort (_,_,GVRepEdge{ gvPartNodes=[] } ) = True
        isShort _ = False

connectEdges :: (TwoDRender c, DynGraph gr) => GVRepGraph gr a b -> TwoDDiagram c -> TwoDDiagram c
connectEdges g d = straitArrows d $ edges g

straitArrows :: (TwoDRender c) => TwoDDiagram c -> [Edge] -> TwoDDiagram c
straitArrows = foldr ( uncurry ( connectOutside' defaultArrowOpt ) )
