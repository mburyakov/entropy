module Main where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Text.Printf

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

entropyN str n =
  (entropy $ ngrams str n) / fromIntegral n

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

printTable1 :: (String, String) -> Map.Map Char Float -> IO ()
printTable1 (h1, h2) table = do
  putStrLn "┌─────┬───────┐"
  printf   "│%4s │%6s │\n" h1 h2
  putStrLn "├─────┼───────┤"
  mapM_ (\(c,f) -> printf "│ '%c' │%6.2f │\n" c f) $ Map.toList table
  putStrLn "└─────┴───────┘"
  putStrLn ""

printTable2 :: (String, String, String) -> [(Int, Float, Float)] -> IO ()
printTable2 (h1, h2, h3) table = do
  putStrLn "┌────┬───────┬───────┐"
  printf   "│%3s │%6s │%6s │\n" h1 h2 h3
  putStrLn "├────┼───────┼───────┤"
  --putStrLn $ show table
  mapM_ (\(c,f1,f2) -> printf "│ %2d │%6.2f │%6.2f │\n" c f1 f2) $ table
  putStrLn "└────┴───────┴───────┘"
  putStrLn ""

main = do
  --putStrLn "Enter your text:"
  str <- getContents
  printTable1 ("x","I(x)") (content str)
  printTable2 ("n","Hn(X)","H(X|.)") (map (\n -> (n, entropyN str n, entropyCondN str n)) [1..10])