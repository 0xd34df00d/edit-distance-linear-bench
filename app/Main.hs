module Main where

import qualified Data.ByteString.Char8 as BS
import Text.EditDistance.Linear01Pure

main :: IO ()
main = do
  let s1 = BS.replicate len 'a'
  let s2 = s1
  let s3 = BS.replicate len 'b'
  print $ levenshteinDistance s1 s2
  print $ levenshteinDistance s1 s3
  where
    len = 20000
