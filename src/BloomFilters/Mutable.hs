module BloomFilters.Mutable
    (
      MutBloom
    , elem
    , notElem
    , insert
    , length
    , new
    ) where

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad (foldM, liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray, mapArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)
import Data.Array.ST (STUArray)


import BloomFilters.Internal (MutBloom(..))



new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = (MB hash) <$> na
    where
        na :: ST s (STUArray s Word32 Bool)
        na = newArray (0, numBits - 1) False


length :: MutBloom s a -> ST s Word32
length (MB _ mutArray) = (succ . snd) <$> (getBounds mutArray)


insert :: MutBloom s a -> a -> ST s ()
insert mb ele = do
    idxs <-indices mb ele
    setAll idxs (mutArray mb)


indices :: MutBloom s a -> a -> ST s [Word32]
indices mb  ele = do
    sz <- length mb
    return $ map (\x -> mod x sz) (mutHash mb ele)


setAll :: [Word32] -> STUArray s Word32 Bool -> ST s ()
setAll ids arr = mapM_ (\i -> writeArray arr i True) (ids)



elem, notElem :: a -> MutBloom s a -> ST s Bool


elem elt filt = indices filt elt >>=
                allM (readArray (mutArray filt))

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x:xs) = do
  ok <- p x
  if ok
    then allM p xs
    else return False
allM _ [] = return True

{--
elem ele mb = do
    idxs <- indices mb ele
    allIdxs idxs (mutArray mb)
--}

notElem ele mb = not <$> elem ele mb

allIdxs :: [Word32] -> STUArray s Word32 Bool -> ST s Bool
allIdxs idxs arr = foldM (fun arr) True idxs


fun :: STUArray s Word32 Bool -> Bool -> Word32 -> ST s Bool
fun arr acc x = do
    v <- readArray arr x
    return $ v && acc
