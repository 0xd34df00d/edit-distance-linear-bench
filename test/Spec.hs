{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Char8 as BS
import qualified Text.EditDistance as TED
import System.IO.Unsafe
import Test.Hspec
import Test.QuickCheck

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
import qualified Text.EditDistance.Cpp as Cpp

testFun :: String -> (BS.ByteString -> BS.ByteString -> Int) -> SpecWith ()
testFun name fun = it (name <> " matches ED") $
  property $ \(getASCIIString -> s1) (getASCIIString -> s2) -> fun (BS.pack s1) (BS.pack s2) `shouldBe` TED.levenshteinDistance TED.defaultEditCosts s1 s2

main :: IO ()
main = hspec $ do
  testFun "L01" L01.levenshteinDistance
  testFun "L01S" L01S.levenshteinDistance
  testFun "L01SL" L01SL.levenshteinDistance
  testFun "L02" L02.levenshteinDistance
  testFun "L02S" L02S.levenshteinDistance
  testFun "L02SL" L02SL.levenshteinDistance
  testFun "L03" L03.levenshteinDistance
  testFun "L03S" L03S.levenshteinDistance
  testFun "L03SL" L03SL.levenshteinDistance
  testFun "L04" L04.levenshteinDistance
  testFun "L04S" L04S.levenshteinDistance
  testFun "L04SL" L04SL.levenshteinDistance
  testFun "L05" L05.levenshteinDistance
  testFun "L06" L06.levenshteinDistance
  testFun "C++" (\s1 s2 -> unsafePerformIO $ Cpp.levenshteinDistance s1 s2)
