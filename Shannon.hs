module Shannon where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

import Analisis
import Code
import Control.Monad

pr str = sortBy (flip compare) $ map swap $ Map.toList $ (freq . count) str

qu str = snd $ mapAccumL (\x (y,_) -> (x+y,x)) 0 (pr str)

frac q =
  q - fromIntegral (floor q)

toBin 0 _ =
  []
toBin n q =
  (floor (2*q) > 0) : toBin (n-1) (frac (2*q))

code str =
  map (\((p,c),q) -> (toBin (ceiling (- logBase 2 p)) q, c)) $ zip (pr str) (qu str)

buildTableFromPhrase str =
  Map.fromList . map swap $ (code str)

data Shannon a = Shannon (TreeCode a)

instance StreamCode Shannon where
  decode code@(Shannon h) str = decode h str >>= (\x -> return (code, snd x))
  encode code@(Shannon h) str = (code, snd $ encode h str)

instance Buildable Shannon where
  build str =
    Shannon $ TreeCode canTable canTree
      where
        table = buildTableFromPhrase str
        tree' = buildTreeFromTable table
        canTable = buildTableFromTree tree'
        canTree =  buildTreeFromTable canTable