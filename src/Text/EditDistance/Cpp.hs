{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Text.EditDistance.Cpp where

import qualified Data.ByteString.Char8 as BS
import Language.C.Inline.Cpp as C

C.context $ C.cppCtx <> C.bsCtx

C.include "<algorithm>"
C.include "<iostream>"
C.include "<numeric>"
C.include "<vector>"
C.include "<string>"

C.verbatim "#pragma GCC optimize (\"O3\")"

levenshteinDistance :: BS.ByteString -> BS.ByteString -> IO Int
levenshteinDistance s1 s2 = fromIntegral <$> [C.block|size_t {
	const auto m = $bs-len:s1;
	const auto n = $bs-len:s2;
  const auto s1 = $bs-ptr:s1;
  const auto s2 = $bs-ptr:s2;

  std::vector<int64_t> v0;
  v0.resize(n + 1);
  std::iota(v0.begin(), v0.end(), 0);

  auto v1 = v0;

  for (size_t i = 0; i < m; ++i)
  {
    v1[0] = i + 1;

    for (size_t j = 0; j < n; ++j)
    {
      auto delCost = v0[j + 1] + 1;
      auto insCost = v1[j] + 1;
      auto substCost = s1[i] == s2[j] ? v0[j] : (v0[j] + 1);

      v1[j + 1] = std::min({ delCost, insCost, substCost });
    }

    std::swap(v0, v1);
  }

  return v0[n];
}|]
