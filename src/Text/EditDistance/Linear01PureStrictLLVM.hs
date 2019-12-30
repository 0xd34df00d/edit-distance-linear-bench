{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fllvm #-}

module Text.EditDistance.Linear01PureStrictLLVM where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import Data.List

levenshteinDistance :: BS.ByteString -> BS.ByteString -> Int
levenshteinDistance s1 s2 = foldl' outer (V.generate (n + 1) id) [0 .. m - 1] V.! n
  where
    m = BS.length s1
    n = BS.length s2

    outer v0 i = V.constructN (n + 1) ctr
      where
        s1char = s1 `BS.index` i
        ctr v1 | V.length v1 == 0 = i + 1
        ctr v1 = min (substCost + substCostBase) $ 1 + min delCost insCost
          where
            j = V.length v1
            delCost = v0 V.! j
            insCost = v1 V.! (j - 1)
            substCostBase = v0 V.! (j - 1)
            substCost = if s1char == s2 `BS.index` (j - 1) then 0 else 1
