{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fllvm #-}

module Text.EditDistance.Linear07TailRecUnsafeNoReadStrictLLVM where

import Data.Foldable (for_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Primitive.Mutable as MV
import Control.Monad.ST

levenshteinDistance :: BS.ByteString -> BS.ByteString -> Int
levenshteinDistance s1 s2 = runST $ do
  v0Init <- MV.unsafeNew n
  v1Init <- MV.unsafeNew n
  for_ [0..n] $ \i -> MV.unsafeWrite v0Init i i
  for_ [0..n] $ \i -> MV.unsafeWrite v1Init i 0
  loop 0 v0Init v1Init
  if even m
  then MV.unsafeRead v0Init n
  else MV.unsafeRead v1Init n

  where
    m = BS.length s1
    n = BS.length s2

    loop :: Int -> MV.MVector s Int -> MV.MVector s Int -> ST s ()
    loop i v0 v1 | i == m = pure ()
                 | otherwise = do
      MV.unsafeWrite v1 0 (i + 1)
      let s1char = s1 `BS.unsafeIndex` i
      let go j prev | j == n = pure ()
                    | otherwise = do
            delCost <- MV.unsafeRead v0 (j + 1)
            substCostBase <- MV.unsafeRead v0 j
            let substCost = if s1char == s2 `BS.unsafeIndex` j then 0 else 1
            let res = min (substCost + substCostBase) $! 1 + min delCost prev
            MV.unsafeWrite v1 (j + 1) res
            go (j + 1) res
      go 0 (i + 1)
      loop (i + 1) v1 v0
