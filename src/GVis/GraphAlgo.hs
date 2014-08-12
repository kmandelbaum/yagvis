module GVis.GraphAlgo where
import Data.Foldable ( maximum, minimum )
import Data.Text.Lazy.IO ( readFile )
import Prelude hiding ( readFile, maximum, minimum )
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Parsing
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.GraphViz (dotToGraph, Attributes)
import Data.Graph.Inductive.Tree
import Data.GraphViz.Attributes.Complete( DirType( Back ), Attribute(Dir) )
import Data.Graph.Inductive.Query.DFS
import Data.Array
--import Data.List hiding (maximum, minimum)
import Data.Tree
import Utility
import Data.Function(on)
import Data.Tuple(swap)

type MyGraph = Gr Attributes Attributes

parser :: Parse ( DotGraph Node )
parser = parse 

loadGraph :: IO ( Either String MyGraph )
loadGraph = do
    file <- readFile "1.dot"
    return $ (dotToGraph <$>) $ fst $ runParser parser file

revertBackEdges :: MyGraph -> MyGraph
revertBackEdges = gmap revertBackEdge

revertBackEdge :: Context a [Attribute] -> Context a [Attribute] 
revertBackEdge (i, n, d, o) = (i', n, d, o')
  where 
    isBack = elem (Dir Back) . fst
    i' = filter isBack o ++ filter (not . isBack) i
    o' = filter isBack i ++ filter (not . isBack) o

removeBackEdges :: MyGraph -> MyGraph
removeBackEdges = elfilter ( notElem (Dir Back) )

markBackEdges :: MyGraph -> MyGraph
markBackEdges g = edgeMap (ifF isBack markBack id) g
  where
    forest = trueDff' g
    allnodes = nodeRange g

    etimes = array allnodes $ concatMap flatten forest `zip` [1..]
    ltimes = array allnodes $ gatherForestLtimes forest

    gatherForestLtimes [] = []
    gatherForestLtimes (Node x ts:ns) = (x, f x ts):gatherForestLtimes (ns ++ ts)

    f x [] = etimes ! x
    f _ ts = maximum $ map ((ltimes !) . rootLabel) ts

    isBack (from, to, _) = (intervalContains `on` timeInterval ) to from
    timeInterval x = (etimes ! x, ltimes ! x)

    markBack (from, to, attrs) = (from, to, Dir Back:attrs)
    
edgeMap :: (DynGraph gr) => (LEdge b -> LEdge c) -> gr a b -> gr a c
edgeMap f = gmap transfmContext
  where 
    transfmContext c = ( map (ledgeFrom . f) (inn' c), node' c, lab' c, map (ledgeTo . f) (out' c) )
    ledgeFrom (from, _, l) = (l, from)
    ledgeTo ( _, to, l) = (l, to)

earlyTimes :: Graph gr => gr a b -> Array Node Int
earlyTimes g = arr
  where
    arr = array ( nodeRange g ) entries
    entries = map ( arrayEntry . context g ) $ nodes g

    arrayEntry ([], n, _, _) = (n, 0)
    arrayEntry c = (n, (1+) $ maximum $ map (arr !) $ filter (n /=) $ pre' c)
      where n = node' c

trueDff' g = dff spawnNodes g
  where spawnNodes = map rootLabel $ filter isInitial forest
        isInitial = all noEdgeFromOtherTree . flatten
        forest = dff' g
        noEdgeFromOtherTree n = all ( == forestId ! n) $ map (forestId !) $ pre g n 
        forestId = array (nodeRange g) $ concat labeledNodes
        labeledNodes = zipWith zip (map flatten forest) $ map repeat [1..]

toLevels :: Graph gr => gr a b -> Array Int [Node]
toLevels g = accumArray (flip (:)) [] (min, max) (map swap $ assocs et)
  where et = earlyTimes g
        (min, max) =  (minimum et, maximum et)

unright (Right x) = x

graph =  revertBackEdges <$> markBackEdges <$> unright <$> loadGraph

