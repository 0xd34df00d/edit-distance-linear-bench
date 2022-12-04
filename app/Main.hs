{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Random.MWC
import System.Environment
import System.IO.Unsafe
import System.Random.MWC

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
import qualified Text.EditDistance.Linear05TailRecUnsafeStrictLLVM as L05
import qualified Text.EditDistance.Linear06TailRecUnsafeNoReadStrictLLVM as L06
import qualified Text.EditDistance.Linear07TailRecUnsafeNoReadStrictLLVM as L07
import qualified Text.EditDistance.Cpp as Cpp

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
           | impl == "5" = L05.levenshteinDistance
           | impl == "6" = L06.levenshteinDistance
           | impl == "7" = L07.levenshteinDistance
           | impl == "C++" = \s1 s2 -> unsafePerformIO $ Cpp.levenshteinDistance s1 s2
           | otherwise = error "Unknown implementation"

  gen <- create
  s1 <- randomGen gen len
  s2 <- randomGen gen len
  s3 <- randomGen gen len
  print $ func s1 s2
  print $ func s1 s3
  where
    len = 20000
