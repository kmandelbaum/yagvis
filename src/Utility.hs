module Utility where
import Control.Monad
import Data.Array
import Control.Arrow

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

ifF :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifF = liftM3 if'

intervalContains :: Ord a => (a,a) -> (a,a) -> Bool
intervalContains (x',y') (x'',y'') = x' <= x'' && y' >= y'' 

(-<-) :: (a -> b, a -> c) -> a -> (b, c)
(-<-) (f, g) a = (f a, g a)
