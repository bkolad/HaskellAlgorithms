module HyperLogLog.HyperLogLog where

import qualified Data.Semigroup as S
import Data.Word
import Data.Monoid
import qualified Data.Bits as B
import Common
import System.Random
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.List

uniques :: (a -> Word32) ->  [a] -> Int
uniques hash ls = 2 ^ maxZeros
    where
        S.Max maxZeros = foldl' (\acc x -> acc <> (zeros x)) mempty ls
        zeros = S.Max . B.countLeadingZeros . hash


main = do
    seed  <- newStdGen
    let rs = randomlist (10 ^9) seed
    print $ uniques id rs

randomlist :: Int -> StdGen -> [Word32]
randomlist n = take n . unfoldr (Just . random)
