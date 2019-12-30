module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Text.EditDistance.Linear01Pure as L01P
import qualified Text.EditDistance.Linear01PureStrict as L01PS
import qualified Text.EditDistance.Linear01PureStrictLLVM as L01PSL
import qualified Text.EditDistance.Linear02PureUnsafe as L02P
import qualified Text.EditDistance.Linear02PureUnsafeStrict as L02PS
import qualified Text.EditDistance.Linear02PureUnsafeStrictLLVM as L02PSL
import System.Environment

main :: IO ()
main = do
  [impl] <- getArgs
  let func | impl == "L01P" = L01P.levenshteinDistance
           | impl == "L01PS" = L01PS.levenshteinDistance
           | impl == "L01PSL" = L01PSL.levenshteinDistance
           | impl == "L02P" = L02P.levenshteinDistance
           | impl == "L02PS" = L02PS.levenshteinDistance
           | impl == "L02PSL" = L02PSL.levenshteinDistance
           | otherwise = error "Unknown implementation"
  let s1 = BS.replicate len 'a'
  let s2 = s1
  let s3 = BS.replicate len 'b'
  print $ func s1 s2
  print $ func s1 s3
  where
    len = 20000
