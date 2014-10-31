{-# LANGUAGE TypeFamilies,TypeSynonymInstances,RankNTypes,FlexibleContexts,ScopedTypeVariables #-}
module GVis.GVis where


import Diagrams.Prelude hiding ( view )
import Diagrams.TwoD.Text as DiaText
import Diagrams.Core.Style

import GVis.GraphAlgo( earlyTimes )
import GVis.MinIntersect ( findChainLeveling, findGoodLeveling )

import Data.Maybe( fromMaybe, fromJust, isJust )
import Data.Default( def )

import Data.Graph.Inductive hiding ( (&), Path )
import Data.IntMap( fromListWith, (!), IntMap )
import Data.Tuple( swap )
import Data.Monoid ( mappend )
import Data.List( partition )
import Data.Foldable( foldMap, toList )
import Data.Sequence( Seq )
import Data.Semigroup.Reducer( unit )

import Control.Arrow( (&&&), (***) )
import Control.Lens( _1, _2, _3, view, traverse, over )

import Debug.Trace as DT

class (Renderable DiaText.Text c, Renderable (Path R2) c, Backend c R2) => TwoDRender c

type TwoDDiagram c = Diagram c R2

vcatopts  = ( with & sep .~ 200.0 )
hcatopts  = ( with & sep .~ 20.0 )

defaultArrowOpt :: ArrowOpts
defaultArrowOpt = with & headLength .~ (Local 40) & shaftStyle .~ (lineWidth thin mempty) & arrowTail .~ noTail


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
            oldEdge = GVRepEdge (Just d) nn

    levels = earlyTimes g
    levelDelta (from, to, _) = levels ! to - levels ! from
    isCrossLevel e = levelDelta e > 1

    crossEdges = filter isCrossLevel $ labEdges g
    removeCrossEdges = efilter ( not . isCrossLevel )

graphToDia :: (DynGraph gr, TwoDRender c) => GVRepGraph gr a b -> TwoDDiagram c
graphToDia g = connectEdges g $ hvcat' hcatopts vcatopts dLevels
  where dLevels = ( map . map ) nodeToDia $ map toList $ toList nLevels

        --nLevels :: IntMap (Seq Node)
        --nLevels = fromListWith mappend acsList
          --where acsList = map ( (gvLevel *** unit) . swap ) $ labNodes g
        nLevels :: [[Node]]
        nLevels = findChainLeveling $ elfilter (null . gvPartNodes) g

        nodeToDia n = named n $ maybe invisNode (const visNode) d
          where GVRepNode{ gvNodeData = d } = fromJust $ lab g n
                invisNode = center $ rect 40 10 # lw none
                visNode = center $ strutX 50 ||| ( ellipseXY 100 60 # lw thin <> text (show n) # fontSize (Local 40) ) ||| strutX 50

--connectShort :: (TwoDRender c, Graph gr) => gr a (GVRepEdge b) -> TwoDDiagram c -> TwoDDiagram c
--connectShort g = straitArrows e
--  where isShort = null . gvPartNodes . view _3
--        e = map ( \(x,y,_) -> (x,y) ) $ filter isShort $ labEdges g

--connectEdges g d = straitArrows d $ edges g
connectEdges :: (TwoDRender c, DynGraph gr) => gr a (GVRepEdge b) -> TwoDDiagram c -> TwoDDiagram c
connectEdges g = connectLong . connectShort
  where connectShort = straitArrows shorts'
        connectLong = splineArrows longs
        (shorts,longs) = partition isShort $ labEdges g
        shorts' = map ( view _1 &&& view _2 ) $ filter ( isJust . gvEdgeData . view _3 ) shorts
        isShort = null . gvPartNodes . view _3

straitArrows :: (TwoDRender c) => [Edge] -> TwoDDiagram c -> TwoDDiagram c
straitArrows = flip $ foldr ( uncurry ( connectOutside' defaultArrowOpt ) )

splineArrows :: (TwoDRender c) => [LEdge (GVRepEdge a)] -> TwoDDiagram c -> TwoDDiagram c
splineArrows es d = d <> foldMap edgeSubDia es
  where 
    edgeSubDia (from, to, GVRepEdge _ partNodes) = arrowBetween' (defaultArrowOpt & arrowShaft .~ shaft) fromEnd toEnd
      where 
        fromEnd = fst $ arrowEnd from $ head partNodes
        (toEnd, toEnd') = arrowEnd to $ last partNodes 
        locs = fromEnd:map (location . getSubDia) partNodes ++ [toEnd']
        getSubDia n = fromJustMsg (lookupName n d) $
            "Error: name " ++ show n ++ " isn't in the diagram"
        arrowEnd n1 n2 = let s1 = getSubDia n1
                             l1 = location s1
                             l2 = location $ getSubDia n2
                             d = l2 .-. l1
                             end = fromMaybe l1 (maxTraceP l1 d s1)
                             adjustment = 40 *^ normalized d
                         in (end, end .+^ adjustment)
        shaft = cubicSpline False locs
fromJustMsg = flip ( fromMaybe . error )
