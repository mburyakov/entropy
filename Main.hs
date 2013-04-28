module Main where

import System.Environment
import qualified Data.Map as Map
import Text.Printf
import Data.List
import Data.Maybe

import Analisis
import Code
import Huffman
import ErrProb

printTable1 :: (String, String, String, String) -> [(String, Int, Float, Float)] -> IO ()
printTable1 (h1, h2, h3, h4) table = do
  putStrLn                    "┌───────┬────────┬──────┬──────┐"
  printf                      "│   %2s  │%7s │ %4s │ %4s │\n" h1 h2 h3 h4
  putStrLn                    "├───────┼────────┼──────┼──────┤"
  mapM_ (\(c,i,p,f) -> printf "│%7s│ %6d │%5.2f │%5.2f │\n" ("'"++c++"'") i p f) table
  putStrLn                    "└───────┴────────┴──────┴──────┘"
  putStrLn ""

printTable2 :: (String, String, String) -> [(Int, Float, Float)] -> IO ()
printTable2 (h1, h2, h3) table = do
  putStrLn                    "┌────┬───────┬───────┐"
  printf                      "│%3s │%6s │%6s │\n" h1 h2 h3
  putStrLn                    "├────┼───────┼───────┤"
  mapM_ (\(c,f1,f2) -> printf "│ %2d │%6.2f │%6.2f │\n" c f1 f2) $ table
  putStrLn                    "└────┴───────┴───────┘"
  putStrLn ""

printTable4 :: (String, String, String) -> [(Int, Float, Float)] -> IO ()
printTable4 =
  printTable2


task1 intsect cnt str = do
  putStrLn $ "Information quantity of symbols in groups of " ++ show cnt ++ ":"
  printTable1 ("x","n(x)","p(x)","I(x)") $ map g $ zip3 (Map.toList $ count str') (Map.toList $ freq $ count str') (Map.toList $ content str')
    where
      g ((a,b),(c,d),(e,f)) | a==c && a==e = (a,b,d,f)
      str' = smartSplit intsect str cnt

task2 intsect rows str = do
  putStrLn $ "Entropy of symbols in groups of n:"
  printTable2 ("n","Hn(X)","H(X|.)") (map (\n -> (n, entropyN intsect str n, entropyCondN str n)) $ take rows [1..])

task3 str = do
  putStrLn $ "Source string: " ++ show str
  putStrLn $ "Length = " ++ show (length str) ++ " symbols."
  putStrLn ""
  if length (nub str) < 2 then
    putStrLn "Error: string should contain at least two distinct symbols."
  else do
    putStrLn $ "Encoded string: " ++ show (map boolToChar encoded)
    putStrLn $ "Length = " ++ show (length encoded) ++ " bits."
    putStrLn ""
    putStrLn $ "Decoded string: " ++ show str'
    putStrLn ""
    putStrLn $ "Compression coefficient: " ++ show ((fromInteger.toInteger) (length str) / (fromInteger.toInteger) (length encoded) * 8)
    where
      (code, encoded) = encodeFile str :: (Huffman Char, [Bool])
      (Just (code', str')) = decodeFile encoded :: Maybe (Huffman Char, [Char])

task4 intsect cnt rows str = do
  putStrLn $ "Probability of error when using code of fixed length N (in groups of " ++ show cnt ++ "):"
  printTable4 ("N","R","Perr") (map (\n -> (n, (fromInteger.toInteger)n/(fromInteger.toInteger)cnt, errProb (smartSplit intsect str cnt) n)) $ take rows [0..])

help progName =
  putStrLn $ "usage:\n\n" ++ progName ++ " [--help] [--verbose] [--count N] [--rows N] [intersect] [all] [analyze] [entropy] [huffman]\n\n"
      ++ "OPTIONS\n\n"
      ++ "  --help       print this help\n"
      ++ "  --verbose    read oneline phrase from keyboard (other case read stdin up to eof)\n"
      ++ "  --count N    group symbols into groups of N (in 'analyze' and 'error'); default 1\n"
      ++ "  --rows N     calculate and print N rows in table ('entropy', 'error'); default 10\n"
      ++ "  --intersect  allow intersections when grouping symbols\n\n"
      ++ "  all          all of tasks described below (like --count 1 --verbose analyze entropy huffman error)\n"
      ++ "  analyze      quantity, probability and information quantity of each symbol of the phrase\n"
      ++ "  entropy      entropy of the phrase (conditional and in groups)\n"
      ++ "  huffman      encode phrase with universal Huffman code\n"
      ++ "  error        calculate error probability when encoding in groups of '--count' with code with fixed various length\n\n"
      ++ "If you are stuck try 'all'\n\n"
      ++ "entropy v.5\n\n"
      ++ "You can free distibute this software, but I will be grateful if you let me know about any bugs or your usage experience.\n"
      ++ "Copyright: Mihail A. Buryakov <mburyakov@dcn.ftk.spbstu.ru>"

checkOpt =
  elem

readOpt opt args def =
  if isJust pos_
  then do
    pos <- pos_
    ans <- if length args>pos+1 then Just $ args !! (pos+1) else Nothing
    return (read ans)
  else
    return def
    where
      pos_ = elemIndex opt args

main =
  getArgs >>= process

process args = do
  cnt <- return $ (readOpt "--count") args 1
  rows <- return $ (readOpt "--rows") args 10
  intsect <- return $ checkOpt "--intersect" args
  if "--help" `elem` args || args == [] || isNothing cnt || isNothing rows
  then do
    progName <- getProgName
    help progName
  else do
    str <- if "--verbose" `elem` args || "all" `elem` args
    then do
      putStrLn "Enter your text:"
      getLine
    else do
      getContents
    if "analyze" `elem` args || "all" `elem` args
    then do
      task1 intsect (fromJust cnt) str
    else do
      return ()
    if "entropy" `elem` args || "all" `elem` args
    then do
      task2 intsect (fromJust rows) str
    else do
      return ()
    if "huffman" `elem` args || "all" `elem` args
    then do
      task3 str
    else do
      return ()
    if "error" `elem` args || "all" `elem` args
    then do
      task4 intsect (fromJust cnt) (fromJust rows) str
    else do
      return ()
    if "--verbose" `elem` args || "all" `elem` args
    then do
      putStrLn "Done. Press <Enter>."
      _ <- getLine
      return ()
    else
      return ()