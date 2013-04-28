module Analisis where

import qualified Data.Map as Map
import Data.Maybe

count str =
  Map.fromListWith (+) [(c,1) | c <- str]

freq cnt =
  Map.map (/ total) cnt
    where
      total = mapSum cnt

mapSum m =
  Map.foldl' (+) 0 m

mapLookupDef d k m =
  fromMaybe d $ Map.lookup k m

mapLookupSure k m =
  fromJust $ Map.lookup k m

content' prob =
  Map.map (\x -> - logBase 2 x) prob

entropy' prob =
  - (mapSum $ Map.map (\x -> x * logBase 2 x) prob)

entropy str =
  (entropy' . freq . count) str

content str =
  (content' . freq . count) str

ngrams str n =
  [take n $ drop i str | i <- [0..length str - n]]

smartSplit intsect =
  if intsect then ngrams else split
  
split str n =
  if length str>0
  then
    take n str : split (drop n str) n
  else
    []

entropyN intsect str n =
  (entropy $ (smartSplit intsect) str n) / fromIntegral n

entropyCond' prob =
  - (mapSum $ Map.mapWithKey (\k p -> p * (logBase 2 (mapLookupSure k cond))) prob)
    where
	  sumy = Map.foldlWithKey' (\condmap (x,y) p -> Map.insertWith' (+) y p condmap) Map.empty prob
	  cond = Map.mapWithKey (\(x,y) p -> p / fromMaybe 0 (Map.lookup y sumy)) prob

ngramToTuple n ngr =
  (x,y)
    where
	  (y,[x]) = splitAt (n-1) ngr

entropyCondN str n =
  (entropyCond' . freq . count) (map (ngramToTuple n) (ngrams str n))