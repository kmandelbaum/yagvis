{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GVis.MinIntersect where
import Data.Graph.Inductive
import GVis.GraphAlgo
import Data.IntMap( IntMap, (!), keys )
import qualified Data.IntMap as IM
import Control.Exception( assert )
import Data.Maybe( isJust, fromJust )
import Data.IntSet( notMember, member, IntSet )
import qualified Data.IntSet as IS
import Control.Lens( sumOf, folded, _2, _1, over, both, view, _3, _4 )
import Control.Arrow( (&&&), first, second )
import Data.List( sort, elemIndex, sortBy, minimumBy, zipWith4, foldl', maximumBy, zip5 )
import Data.Functor ( fmap, (<$>) )
import Data.Function( on )
import Prelude
import Utility( ifF, if' )
import Data.Ord( comparing, Down( Down ) )
import Data.Tree( flatten )
import qualified Data.Array as A
import Data.Function.Memoize

import qualified Debug.Trace as DT

dt x = DT.trace (show x) x

-- be type-safe about Pos and Score.
-- NB: Node still equals Int as a type ( to use IntMap easily )
--
newtype Score = Score Int deriving ( Show, Num, Eq, Ord, Enum )
newtype Pos = Pos Int deriving ( Show, Num, Eq, Ord, Enum )

data Leveling = Leveling { levScore ::  Score,
                           levSet :: IntSet,
                           levNodePos :: IntMap (IntMap Pos) }
    deriving ( Show )

findGoodLeveling :: Graph gr => gr a b -> [[Node]]
findGoodLeveling = levelingToNodes . findGoodLeveling'

findGoodLeveling' :: Graph gr => gr a b -> Leveling
findGoodLeveling' g = leveling
  where
    --
    -- ns = sortBy (comparing (Down . deg g)) $ nodes g
    -- ns = concatMap flatten $ trueDff' g
    ns = reverse $ nodes g
    levMap = earlyTimes g
    maxLevel = maximum $ IM.elems levMap
    initNodePos = IM.fromList $ zip [0..maxLevel] $ repeat IM.empty
    initLeveling = Leveling 0 IS.empty initNodePos
    leveling = foldl (flip $ insertNode levMap g) initLeveling ns

findChainLeveling' :: Graph gr => gr a b -> Leveling
findChainLeveling' g = leveling
  where
    ns = nodes g
    levMap = earlyTimes g
    maxLevel = maximum $ IM.elems levMap
    chains :: [[Node]]
    chains = sortBy ( comparing (Down . length) ) $ chainDecomposeOpt g
    initLeveling = Leveling 0 IS.empty initNodePos
    initNodePos = IM.fromList $ zip [-1..maxLevel+1] $ repeat IM.empty
    leveling'= foldl' ( flip $ insertChain levMap g) initLeveling chains
    leveling'' = foldl' ( flip $ reinsertChain levMap g) leveling' chains
    leveling = leveling''{ levNodePos = IM.delete (-1) $ IM.delete (maxLevel+1) $ levNodePos leveling''}

findChainLeveling :: Graph gr => gr a b -> [[Node]]
findChainLeveling = levelingToNodes . findChainLeveling'

insertNode :: Graph gr => IntMap Int -> gr a b -> Node ->  Leveling -> Leveling
insertNode gLevMap g n l@(Leveling lScore lSet lNodePos) =
    assert precond $ Leveling newScore newSet newNodePos
  where
    precond = isJust (lab g n) && notMember n lSet
    myLevel = gLevMap ! n
    curNodePos = lNodePos ! myLevel
    siblings = ascByValKeys curNodePos

    dirScr goDir = sumRLCosts nbrsScr
      where nbrsPos = sort . map ( posMap ! ) . filter ( `IS.member` lSet) . dirFun
            nbrsScr = fmap (on rightLeftScore nbrsPos n) siblings
            (adjLevel, dirFun) = if goDir then (pred myLevel, pre g) else (succ myLevel, suc g)
            posMap = lNodePos ! adjLevel
    scores = on (zipWith (+)) dirScr True False
    bestScore = minimum scores
    bestPos = Pos $ fromJust $ elemIndex bestScore scores
    newCurNodePos = IM.insert n bestPos $ IM.map ( ifF ( >= bestPos) succ id ) curNodePos
    newNodePos = IM.insert myLevel newCurNodePos lNodePos
    newScore = lScore + bestScore
    newSet = IS.insert n lSet

removeChain :: Int -> [Node] -> Leveling -> Leveling
removeChain firstLevel chain l@(Leveling lScore lSet lPosMap) = Leveling newScore newSet newPosMap
  where
    lastLevel = firstLevel + length chain - 1
    levels = [firstLevel..lastLevel]

    (untHead, tmpTail) = span ( ( < firstLevel) . fst ) $ IM.toList lPosMap
    (modMiddle,untTail) = span ( ( <= lastLevel ) . fst ) tmpTail

    newMiddle = zip levels newMaps
    newPosMap = IM.fromList $ untHead ++ newMiddle ++ untTail
    newMaps = zipWith removeFromLevel chain levels

    removeFromLevel n level = removeFromMap n (lPosMap ! level)
    removeFromMap n m = IM.map ( ifF ( > p ) pred id ) $ IM.delete n m

      where p = m ! n
    newSet = foldr IS.delete lSet chain
    -- TODO: calculate score
    newScore = lScore

reinsertChain :: Graph gr => IntMap Int ->
                             gr a b ->
                             [Node] ->
                             Leveling ->
                             Leveling
reinsertChain levMap g chain =
  insertChain levMap g chain . removeChain firstLevel chain
    where firstLevel = levMap ! (head chain)

insertChain :: Graph gr => IntMap Int ->
                           gr a b ->
                           [Node] ->
                           Leveling ->
                           Leveling
insertChain levMap g chain l@(Leveling lScore lSet lPosMap) = Leveling newScore newSet newPosMap
  where
    firstLevel = levMap ! head chain
    lastLevel = levMap ! last chain
    levels = [firstLevel..lastLevel]

    adjPos dir level = map ( adjPosForNode dir level ) $ ascByValKeys (lPosMap ! level)
    adjPosForNode dir level = sort . map ( ( lPosMap ! levFun level ) ! ) . filtNodes . dirFun
      where
        (levFun, dirFun) = if' dir (pred, pre g) (succ, suc g)

    filtNodes = filter ( `IS.member` lSet)

    nodesAdjPos dir = zipWith (adjPosForNode dir) levels chain
    levelsAdjPos dir = map (adjPos dir) levels
    numSiblings = tail $ map (IM.size . (lPosMap !)) levels

    addCosts = zipWith4 insCostsForNode (nodesAdjPos True) (levelsAdjPos True)
                                        (nodesAdjPos False) (levelsAdjPos False)

    processLevel ( prevNbrsPos, numSib, addCost, prevSucPos, prevPos ) =
        zipWith ( first . (+) ) addCost . insertingCosts' prevNbrsPos numSib prevSucPos prevPos


    levelDesc = zip5 (levelsAdjPos False) numSiblings (tail addCosts) (nodesAdjPos False) (tail $ nodesAdjPos True)
    initScore = zip (head addCosts) (map (:[]) [0..])
    finalScore = minimumBy (comparing fst) $ foldl' (flip processLevel) initScore levelDesc

    insNodeToLevel n p = IM.insert n p . IM.map ( ifF ( >= p) succ id )

    (untHead, tmpTail) = span ( ( < firstLevel) . fst ) $ IM.toList lPosMap
    (modMiddle,untTail) = span ( ( <= lastLevel ) . fst ) tmpTail
    newMiddle = zipWith second (zipWith insNodeToLevel chain (reverse $ snd finalScore)) modMiddle
    newPosMap = IM.fromList $ untHead ++ newMiddle ++ untTail

    newScore = lScore + fst finalScore
    newSet = IS.union lSet $ IS.fromList chain

-- Inserting costs for node in the chain based on
-- prev node inserting costs - count only chain edge intersections

insertingCosts' :: [[Pos]] -> Int -> [Pos] -> [Pos] -> [(Score,[Pos])] -> [(Score,[Pos])]
insertingCosts' prevLevelNbrsPos numSiblings prevSucPos prevPos prevCosts = zipWith ( second . (:) ) [0..] newCosts
  where
    newCosts = map ( minimumBy ( comparing fst ) . zipWith ( flip (first . (+))) prevCosts ) costMatrix

    posToInsert = [0..Pos numSiblings]
    costMatrix = map insCostsForPos posToInsert

    toLefts = map ( \x -> length ( filter ( < x ) prevPos ) ) [0..]
    toRights = map ( length prevPos - ) toLefts

    insCostsForPos p = zipWith (+) addScores $ sumRLCosts (map prevLvlInter prevLevelNbrsPos)
      where
        -- this is unfortunately linear
        -- TODO: can be done in O(1)
        prevLvlInter xs = let ll = length (filter ( < p ) xs) in
                              over both Score (ll, length xs - ll)
        toLeft = length $ filter ( < p ) prevSucPos
        toRight = length prevSucPos - toLeft
        -- additional scores for chain intersections introduced by itself
        addScores = zipWith ( \x y -> Score $ toLeft * x + toRight * y ) toLefts toRights

-- Inserting costs for node into leveling (one adj level).
-- first arg is node's neighbors pos
-- second arg is adjacent level's neighbors pos,
-- ( in the same direction as the adjacent levels )

insertingCosts'' :: [Pos] -> [[Pos]] -> [Score]
insertingCosts'' nbrsPos = sumRLCosts . map (rightLeftScore nbrsPos)

insCostsForNode :: [Pos] -> [[Pos]] -> [Pos] -> [[Pos]] -> [Score]
insCostsForNode n1 nn1 n2 nn2 = zipWith (+) (insertingCosts'' n1 nn1) (insertingCosts'' n2 nn2)

sumRLCosts :: [(Score, Score)] -> [Score]
sumRLCosts rlCosts = scanr ( (+) . uncurry (-) ) s rlCosts
 where s = sumOf ( folded . _2 ) rlCosts

rightLeftScore :: [Pos] -> [Pos] -> (Score, Score)
rightLeftScore init_xs init_ys = over both Score $ calc' init_xs init_ys 0
  where
    len = length init_ys
    calc' [] _ _ = (0,0)
    calc' xxs [] _ = (length xxs * len, 0)
    calc' xxs@(x:xs) yys@(y:ys) toLeft
      | x > y = calc' xxs ys (toLeft+1)
      | x < y = let (lscore', rscore') = calc' xs yys toLeft
                 in ( lscore'+toLeft, rscore'+len-toLeft)
      | x == y = let ( lscore', rscore') = calc' xxs' yys' (toLeft+ylen)
                  in (lscore'+toLeft * xlen, (rscore'+len-toLeft-ylen) * xlen)
        where (xlen, xxs') = first length $ span ( == x ) xxs
              (ylen, yys') = first length $ span ( == y ) yys

--TODO: move some funstions ( not related to instersections) to Leveings.hs

ascByValKeys :: (Ord a) => IntMap a -> [Int]
ascByValKeys = map fst . sortBy (comparing snd) . IM.toList

levelingToNodes :: Leveling -> [[Node]]
levelingToNodes = map ( map fst . sortBy ( comparing snd ) . IM.toList ) . IM.elems . levNodePos

nodesToLeveling :: [[Node]] -> Leveling
nodesToLeveling n = Leveling{ levScore = 0, levSet = lSet, levNodePos = lPosMap }
  where
    lSet = IS.fromList $ concat n
    toMap = IM.fromList . zip [0..]
    lPosMap = toMap $ map (IM.fromList . flip zip [0..] ) n

chainDecompose :: Graph gr => gr a b -> [[Node]]
chainDecompose = chainDecompose' selChain
  where
    selChain g = go $ head $ filter ( (==0) . indeg g ) $ nodes g
      where go n = let s = suc g n in if null s then [n] else n:go (head s)

chainDecompose' :: Graph gr => ( gr a b -> [Node] ) -> gr a b -> [[Node]]
chainDecompose' selector = decomp
  where decomp g' | isEmpty g' = []
                  | otherwise = let ns = selector g' in ns : decomp (delNodes ns g')

chainDecomposeOpt :: Graph gr => gr a b -> [[Node]]
chainDecomposeOpt g = decomp $ IS.fromList $ nodes g
  where
    decomp :: IntSet -> [[Node]]
    decomp s | IS.null s = []
             | otherwise = let ns = selectChain g s in ns : decomp (foldr (IS.delete) s ns)

selectChain :: Graph gr => gr a b -> IntSet -> [Node]
selectChain g nodeSet = view _4 $ maximum $ map score ns
  where
    ns = IS.toList nodeSet
    sucFun = filter ( `IS.member` nodeSet ) . suc g
    score = memoFix score'
    score' :: ( Node -> (Score, Score, Int, [Node]) ) -> Node -> (Score, Score, Int, [Node])
    score' f n =  dpAdjust $ maximum $ (0,0,0,[]):map f (sucFun n)
      where
        --nodeScore = Score $ deg g n
        nodeScore1 = Score $ length $ filter (not . (`IS.member` nodeSet)) nbrs
        nodeScore2 = 0 -- Score $ length nbrs
        nbrs = neighbors g n
        dpAdjust (s1,s2,l,ns) = (s1 + nodeScore1, s2 + nodeScore2, l + 1, n:ns)
