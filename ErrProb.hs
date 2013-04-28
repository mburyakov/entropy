module ErrProb where

import qualified Data.Map as Map
import Data.Tuple
import Data.List

import Analisis

freqList str =
  (sort . map swap . Map.toList . freq . count) str

errProb str n =
  sum $ map fst $ take (len - 2^n) fl
    where
      fl = freqList str
      len = length fl