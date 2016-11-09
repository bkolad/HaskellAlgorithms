module Main where

import qualified BloomFilters.Easy as BF

ls :: [Int]
ls = [1,2,3,4,5]

main = do
    let ebl = BF.easyList 0.1 ls
    case ebl of
        Left err -> print err
        Right bl -> print $ BF.elem 9 bl
