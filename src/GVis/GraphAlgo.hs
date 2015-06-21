module GVis.GraphAlgo where
import Data.Foldable ( maximum, minimum )
import Prelude hiding ( maximum, minimum )
import Data.GraphViz( Attributes )
import Data.GraphViz.Types.Canonical( DotGraph )
import Data.GraphViz.Attributes.Complete( DirType( Back ), Attribute(Dir) )

import Data.Graph.Inductive.Query.DFS ( dff, dff' )
import Data.Graph.Inductive( pre, pre', suc', Graph, node', nodes, context, out', inn', lab', gmap, LEdge,
                             nodeRange, elfilter, DynGraph, Context, Node,
                             labEdges, insEdges)
import Data.Function(on)
import Data.Tuple(swap)
import Data.Function.Memoize( memoFix )
import Data.Tree( Tree( Node), flatten, rootLabel, Forest )
import Data.IntMap( fromList, (!), assocs, IntMap, fromListWith )
import Data.Semigroup.Reducer( unit )
import Data.Monoid ( mappend )
import Control.Lens (over, _3, _2)
import Control.Comonad ( extend )
import Data.Functor ( (<$>) )
import Utility( ifF )
import Numeric.Interval( (...) )
import qualified Numeric.Interval as Inter

revertBackEdges :: (DynGraph gr) => gr a Attributes -> gr a Attributes
revertBackEdges = gmap revertBackEdge

revertBackEdge :: Context a [Attribute] -> Context a [Attribute]
revertBackEdge (i, n, d, o) = (i', n, d, o')
  where
    isBack = elem (Dir Back) . fst
    i' = filter isBack o ++ filter (not . isBack) i
    o' = filter isBack i ++ filter (not . isBack) o

removeBackEdges :: DynGraph gr => gr a Attributes -> gr a Attributes
removeBackEdges = elfilter ( notElem (Dir Back) )

unmarkBackEdges :: DynGraph gr => gr a Attributes -> gr a Attributes
unmarkBackEdges = edgeMap (over _3 $ filter ( /= Dir Back))

markBackEdges :: DynGraph gr => gr a Attributes -> gr a Attributes
markBackEdges g = edgeMap (ifF isBack markBack id) g
  where
    forest = dff' g
    allnodes = nodeRange g

    etimes = fromList $ concatMap flatten forest `zip` [1..]
    ltimes = fromList $ concatMap flatten $ map ( extend f ) forest

    f (Node x []) = (x, etimes ! x)
    f (Node x ts) = (x, maximum $ map ( ( ltimes ! ) . rootLabel ) ts )

    isBack (from, to, _) = ( Inter.contains `on` timeInterval ) to from
    timeInterval x = etimes ! x ... ltimes ! x

    markBack (from, to, attrs) = (from, to, Dir Back:attrs)

edgeMap :: (DynGraph gr) => (LEdge b -> LEdge c) -> gr a b -> gr a c
edgeMap f = gmap transfmContext
  where
    transfmContext c = ( map (ledgeFrom . f) (inn' c), node' c, lab' c, map (ledgeTo . f) (out' c) )
    ledgeFrom (from, _, l) = (l, from)
    ledgeTo ( _, to, l) = (l, to)

earlyTimes :: Graph gr => gr a b -> IntMap Int
earlyTimes g = arr
  where
    arr = fromList entries
    entries = map ( arrayEntry . context g ) $ nodes g

    arrayEntry ([], n, _, _) = (n, 0)
    arrayEntry c = (n, (1+) $ maximum $ map (arr !) $ filter (n /=) $ pre' c)
      where n = node' c

lateTimes :: Graph gr => gr a b -> IntMap Int -> IntMap Int
lateTimes g et = arr
  where
    arr = fromList entries
    entries = map ( arrayEntry . context g ) $ nodes g

    arrayEntry (_, n, _, []) = (n, et ! n)
    arrayEntry c = (n, (subtract 1) $ minimum $ map (arr !) $ filter (n /=) $ suc' c)
      where n = node' c
{-
trueDff' :: Graph gr => gr a b -> Forest Node
trueDff' g = dff spawnNodes g
  where spawnNodes = filter noEdgeFromOtherTree $ map rootLabel forest
        forest = dff' g
        noEdgeFromOtherTree n = all ( == forestId ! n) $ map (forestId !) $ pre g n
        forestId = fromList $ concat labeledNodes
        labeledNodes = zipWith zip (map flatten forest) $ map repeat [1..]
-}

toLevels :: Graph gr => gr a b -> IntMap [Node]
toLevels g = fromListWith mappend (map (over _2 unit . swap) $ assocs $ (earlyTimes g))

-- make the graph acyclic
-- dummy implemetation:
--  looses initial back edge hints (TODO)
-- removes self-edges
prepareGraph :: DynGraph gr => gr Attributes Attributes -> gr Attributes Attributes
prepareGraph g = revertBackEdges $
                 markBackEdges $
                 removeSelfEdges $
                 unmarkBackEdges g
removeSelfEdges :: DynGraph gr => gr a b -> gr a b
removeSelfEdges = gmap removeSelfEdge
  where
    removeSelfEdge (i, n, l, o) = (i', n, l, o')
      where
        i' = filter ( (/= n) . snd ) i
        o' = filter ( (/= n) . snd ) o
