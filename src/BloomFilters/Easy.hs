module BloomFilters.Easy
    ( easyList
    , IM.elem
    , Bloom
    )where

import BloomFilters.Immutable as IM
import Data.Hashable
import Data.List
import Data.Maybe

doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [intToWrod32 (h1 + h2 * i) | i <- [0..numHashes]]
    where
        h1 = hashWithSalt 7500332 value
        h2 = hashWithSalt 236870912 value

intToWrod32 :: Int -> Word32
intToWrod32 i = fromIntegral i

easyList :: (Hashable a)
         => Double        -- false positive rate (between 0 and 1)
         -> [a]           -- values to populate the filter with
         -> Either String (IM.Bloom a)
easyList errRate values =
    case suggestSizing (genericLength values) errRate of
      Left err            -> Left err
      Right (bits,hashes) -> Right filt
        where filt = IM.fromList (doubleHash hashes) bits values



suggestSizing
    :: Integer       -- expected maximum capacity
    -> Double        -- desired false positive rate
    -> Either String (Word32,Int) -- (filter size, number of hashes)
suggestSizing capacity errRate
    | capacity <= 0                = Left "capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes               = Left "capacity too large"
    | otherwise                    = Right (minimum saneSizes)
  where
        saneSizes :: [(Word32, Int)]
        saneSizes = catMaybes . map sanitize $ sizings capacity errRate
        sanitize (bits,hashes)
          | bits > maxWord32 - 1 = Nothing
          | otherwise            = Just (ceiling bits, truncate hashes)
          where maxWord32 = fromIntegral (maxBound :: Word32)

sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
    [(((-k) * cap / log (1 - (errRate ** (1 / k)))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
