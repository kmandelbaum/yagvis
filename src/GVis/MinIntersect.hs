module GVis.MinIntersect where
import Data.Graph.Inductive
import GVis.GraphAlgo
import Data.IntMap( IntMap, (!), keys )
import qualified Data.IntMap as IM
import Control.Exception( assert )
import Data.Maybe( isJust, fromJust )
import Data.IntSet( notMember, member, IntSet )
import qualified Data.IntSet as IS
import Control.Lens( sumOf, folded, _2, _1 )
import Control.Arrow( (&&&), first )
import Data.List( sort, elemIndex, sortBy )
import Data.Functor ( fmap, (<$>) )
import Data.Function( on )
import Prelude
import Utility( ifF )
import Data.Ord( comparing )

type Score = Int

data Leveling = Leveling { levScore ::  Score, 
                           levSet :: IntSet,
                           levNodePos :: IntMap (IntMap Int) }
    deriving ( Show )

findGoodLeveling :: Graph gr => gr a b -> [[Node]]
findGoodLeveling g = map ( map fst . sortBy ( comparing snd ) . IM.toList ) $ IM.elems $ levNodePos leveling
  where
    ns = nodes g
    levMap = earlyTimes g
    maxLevel = maximum $ IM.elems levMap
    initNodePos = IM.fromList $ zip [0..maxLevel] $ repeat IM.empty
    initLeveling = Leveling 0 IS.empty initNodePos
    leveling = foldr (insertNode levMap g) initLeveling ns
    
insertNode :: Graph gr => IntMap Int -> gr a b -> Node ->  Leveling -> Leveling
insertNode gLevMap g n l@(Leveling lScore lSet lNodePos) = 
    assert precond $ Leveling newScore newSet newNodePos
  where 
    precond = (isJust $ lab g n) && (notMember n lSet)
    myLevel = gLevMap ! n
    curNodePos = lNodePos ! myLevel
    siblings = map fst $ sortBy (comparing snd) $ IM.toList curNodePos

    dirScr goDir = scanr ( (+) . uncurry (-) ) s nbrsScr
      where nbrsPos = sort . map ( posMap ! ) . filter (flip member lSet) . dirFun
            nbrsScr = fmap (on rightLeftScore nbrsPos n) siblings
            s = sumOf ( folded . _2 ) nbrsScr
            (adjLevel, dirFun) = if goDir then (pred myLevel, pre g) else (succ myLevel, suc g)
            posMap = lNodePos ! adjLevel
    scores = on (zipWith (+)) dirScr True False
    bestScore = minimum scores
    bestPos = fromJust $ elemIndex bestScore scores 
    newCurNodePos = IM.insert n bestPos $ IM.map (ifF ( >= bestPos) succ id ) curNodePos
    newNodePos = IM.insert myLevel newCurNodePos lNodePos
    newScore = lScore + bestScore
    newSet = IS.insert n lSet
    
rightLeftScore :: [Int] -> [Int] -> (Int, Int)
rightLeftScore init_xs init_ys = calc' init_xs init_ys 0
  where
    len = length init_ys
    calc' :: [Int] -> [Int] -> Int -> (Int, Int)
    calc' [] _ _ = (0,0)
    calc' xxs [] _ = (length xxs * len, 0)
    calc' xxs@(x:xs) yys@(y:ys) toLeft
      | x > y = calc' xxs ys (toLeft+1)
      | x < y = let (lscore', rscore') = calc' xs yys toLeft in ( lscore'+toLeft, rscore'+len-toLeft)
      | x == y = let ( lscore', rscore') = calc' xxs' yys' (toLeft+ylen) in (lscore'+toLeft * xlen, (rscore'+len-toLeft-ylen) * xlen)
        where (xlen, xxs') = first length $ span ( == x ) xxs
              (ylen, yys') = first length $ span ( == y ) yys
