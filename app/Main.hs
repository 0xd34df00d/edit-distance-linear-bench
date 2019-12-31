module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Text.EditDistance.Linear01Pure as L01
import qualified Text.EditDistance.Linear01PureStrict as L01S
import qualified Text.EditDistance.Linear01PureStrictLLVM as L01SL
import qualified Text.EditDistance.Linear02PureUnsafe as L02
import qualified Text.EditDistance.Linear02PureUnsafeStrict as L02S
import qualified Text.EditDistance.Linear02PureUnsafeStrictLLVM as L02SL
import qualified Text.EditDistance.Linear03Array as L03
import qualified Text.EditDistance.Linear03ArrayStrict as L03S
import qualified Text.EditDistance.Linear03ArrayStrictLLVM as L03SL
import qualified Text.EditDistance.Linear04TailRec as L04
import qualified Text.EditDistance.Linear04TailRecStrict as L04S
import qualified Text.EditDistance.Linear04TailRecStrictLLVM as L04SL
import System.Environment

main :: IO ()
main = do
  [impl] <- getArgs
  let func | impl == "1" = L01.levenshteinDistance
           | impl == "1S" = L01S.levenshteinDistance
           | impl == "1SL" = L01SL.levenshteinDistance
           | impl == "2" = L02.levenshteinDistance
           | impl == "2S" = L02S.levenshteinDistance
           | impl == "2SL" = L02SL.levenshteinDistance
           | impl == "3" = L03.levenshteinDistance
           | impl == "3S" = L03S.levenshteinDistance
           | impl == "3SL" = L03SL.levenshteinDistance
           | impl == "4" = L04.levenshteinDistance
           | impl == "4S" = L04S.levenshteinDistance
           | impl == "4SL" = L04SL.levenshteinDistance
           | otherwise = error "Unknown implementation"
  let s1 = BS.replicate len 'a'
  let s2 = s1
  let s3 = BS.replicate len 'b'
  print $ func s1 s2
  print $ func s1 s3
  where
    len = 20000
