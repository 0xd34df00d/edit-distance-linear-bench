{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Criterion.Main

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
main = defaultMain
  [ mkGroup "Pure" L01.levenshteinDistance L01S.levenshteinDistance L01SL.levenshteinDistance
  , mkGroup "Pure (unsafe idx)" L02.levenshteinDistance L02S.levenshteinDistance L02SL.levenshteinDistance
  , mkGroup "Arr" L03.levenshteinDistance L03S.levenshteinDistance L03SL.levenshteinDistance
  , mkGroup "Arr/tailrec" L04.levenshteinDistance L04S.levenshteinDistance L04SL.levenshteinDistance
  , mkBench "Arr/tailrec/unsafe + strict + LLVM" L05.levenshteinDistance
  , mkBench "Arr/tailrec/unsafe + strict + LLVM + no read" L06.levenshteinDistance
  , mkBench "Vec/tailrec/unsafe + strict + LLVM + no read" L07.levenshteinDistance
  , bench "C++ FFI" $ nfAppIO (\(s1, s2, s3) -> (,) <$> Cpp.levenshteinDistance s1 s2 <*> Cpp.levenshteinDistance s1 s3) (s1', s2', s3')
  ]
  where
    mkBench name func = bench name $ nf (\(s1, s2, s3) -> (func s1 s2, func s1 s3)) (s1', s2', s3')
    mkGroup name f1 f2 f3 = bgroup name $ uncurry mkBench <$> [ ("base", f1)
                                                              , ("strict", f2)
                                                              , ("strict + LLVM", f3)
                                                              ]

    num = 20000
    s1' = BS.replicate num 'a'
    s2' = s1'
    s3' = BS.replicate num 'b'
