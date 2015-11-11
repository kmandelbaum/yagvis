module GVis.IsotonicRegression where

type Block a = (a, Int)

av :: Fractional a => Block a -> a
av (x, w) = x / fromIntegral w

weight :: Block a -> Int
weight = snd

mkBlock :: a -> Int -> Block a
mkBlock = (,)

merge :: Num a => Block a -> Block a -> Block a
merge (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isotonicRegression :: (Ord a, Fractional a) => [a] -> [a]
isotonicRegression x = go [] (map (flip mkBlock 1) x)
  where 
    go l [] = reverse $ concatMap (\x->replicate (weight x) (av x)) l
    go [] (r:rs) = go [r] rs
    go (l:ls) (r:rs) = if av l > av r then go ls (merge l r:rs) else go (r:l:ls) rs
