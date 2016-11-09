module BloomFilters.Immutable
    ( Bloom
    , Word32
    , Word8
    , elem
    , fromList) where


import BloomFilters.Internal
import BloomFilters.Mutable (insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32, Word8)
import Prelude hiding (elem, length, notElem)


length :: Bloom a -> Int
length = fromIntegral . len

len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray


elem :: a -> Bloom a -> Bool
elem a b  =
    all (\x -> (blmArray b) ! x) indices
    where
        sz = len b
        indices =  map (\x-> mod x sz) (blmHash b a)


notElem :: a -> Bloom a -> Bool
notElem a b = not $ elem a b


fromList :: (a -> [Word32])    -- family of hash functions to use
         -> Word32             -- number of bits in filter
         -> [a]                -- values to populate with
         -> Bloom a
fromList hash sz vs =
    B hash $ runSTUArray makeArray
    where
        makeArray  = do
            mutB <- new hash sz
            mapM (insert mutB) vs
            return $ mutArray mutB
