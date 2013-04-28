module ErrProb where

import qualified Data.Map as Map
import Data.Tuple
import Data.List

import Analisis

freqList str 1 =
  freqList' $ (map (:[])) str
freqList str l | l>1 = do
  (f1,c1) <- fl
  (f2,c2) <- fl'
  return (f1*f2,c1:c2)
    where
      fl = freqList' str
      fl' = freqList str (l-1)

freqList' str =
  (sort . map swap . Map.toList . freq . count) str


errProb str n l =
  sum $ map fst $ take (len - 2^n) fl
    where
      fl = freqList str l
      len = length fl