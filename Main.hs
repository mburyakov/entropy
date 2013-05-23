module Main where

import System.Environment
import qualified Data.Map as Map
import Text.Printf
import Data.List
import Data.Maybe
import Control.Monad
import Data.Char

import Analisis
import Code
import Huffman
import Shannon
import ErrProb
import LZW

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

printCode str encoded decoded = do
  putStrLn $ "Source string: " ++ show str
  putStrLn $ "Length = " ++ show (length str) ++ " symbols."
  if length (nub str) < 2 then
    putStrLn "Error: string should contain at least two distinct symbols."
  else do
    putStrLn $ "Encoded string: " ++ show (map boolToChar encoded)
    putStrLn $ "Length = " ++ show (length encoded) ++ " bits."
    putStrLn $ "Decoded string: " ++ show decoded
    putStrLn $ "Compression coefficient: " ++ show ((fromInteger.toInteger) (length str) / (fromInteger.toInteger) (length encoded) * 8) ++ "\n"



taskHuffman str = do
  putStrLn "Demonstrating Huffman code:"
  printCode str encoded decoded
    where
      code = build str :: Huffman Char
      encoded      = encodeStr code str
      Just decoded = decodeStr code encoded

taskUniHuffman str = do
  putStrLn "Demonstrating universal Huffman code:"
  printCode str encoded str'
    where
      (code, encoded) = encodeFile str :: (Huffman Char, [Bool])
      (Just (code', str')) = decodeFile encoded :: Maybe (Huffman Char, [Char])

taskShannon str = do
  putStrLn "Demonstrating Shannon code:"
  printCode str encoded decoded
    where
      code = build str :: Shannon Char
      encoded      = encodeStr code str
      Just decoded = decodeStr code encoded

taskLZW str = do
  putStrLn "Demonstrating LZW code:"
  printCode str encoded decoded
    where
      code = build str :: LZW Char
      encoded      = encodeStr code str
      Just decoded = decodeStr code encoded


testStreamCode code str = do
  Just str == decoded
    where
      encoded = encodeStr code str
      decoded = decodeStr code encoded

testUniHuffman str = do
  Just str == liftM snd decoded
    where
      (code, encoded) = encodeFile str :: (Huffman Char, [Bool])
      decoded = decodeFile encoded :: Maybe (Huffman Char, [Char])

testCodes str =
     testStreamCode lzw str
  && testStreamCode huffman str
  && testStreamCode shannon str
  && testUniHuffman str
    where
      lzw = build str :: LZW Char
      shannon = build str :: Shannon Char
      huffman = build str :: Huffman Char

taskTestCodes str = do
  putStrLn "testing Huffman, universal Huffman, Shannon and LZW codes:"
  putStrLn $ if testCodes str then "OK, test passed." else "Test failed. Please, report a bug."

task4 intsect withMem cnt rows str = do
  putStrLn $ "Probability of error when using code of fixed length N (in groups of " ++ show cnt ++ "):"
  printTable4 ("N","R","Perr") (map (\n -> (n, (fromInteger.toInteger)n/(fromInteger.toInteger)cnt, ep n)) $ take rows [0..])
    where
      ep n = if withMem then errProb (smartSplit intsect str cnt) n 1 else errProb str n cnt

convertToAscii str =
  map (\c -> if ord c > 255 then chr (128 + ord c `mod` 128) else c) str

help progName = do
  putStrLn $ "usage:\n\n" ++ progName ++ " [--help] [--verbose] [--count N] [--rows N] [--intersect] [--mem] [all] [analyze] [entropy] [huffman] [unihuffman] [shannon] [error] [lzw] [testcodes]\n\n"
      ++ "OPTIONS\n\n"
      ++ "  --help       print this help\n"
      ++ "  --verbose    read oneline phrase from keyboard (other case read stdin up to eof)\n"
      ++ "  --count N    group symbols into groups of N (in 'analyze' and 'error'); default 1\n"
      ++ "  --rows N     calculate and print N rows in table ('entropy', 'error'); default 10\n"
      ++ "  --intersect  allow intersections when grouping symbols ('analyze', 'entropy', 'error')\n\n"
      ++ "  --mem        assume source having memory ('error')\n\n"
      ++ "  all          all of tasks described below (like --count 1 --verbose analyze entropy huffman unihuffman shannon error lzw)\n"
      ++ "  analyze      quantity, probability and information quantity of each symbol of the phrase\n"
      ++ "  entropy      entropy of the phrase (conditional and in groups)\n"
      ++ "  huffman      encode phrase with universal Huffman code\n"
      ++ "  unihuffman      encode phrase with universal Huffman code\n"
      ++ "  shannon      encode phrase with Shannon code\n"
      ++ "  lzw          encode phrase with LZW code\n"
      ++ "  testcodes    test all build-in codes\n"
      ++ "  error        calculate error probability when encoding in groups of '--count' with code with fixed various length\n\n"
      ++ "If you are stuck try 'all'\n\n"
      ++ "entropy v.7.2\n\n"
      ++ "You can free distibute this software, but I will be grateful if you let me know about any bugs or your usage experience.\n"
      ++ "Copyright: Mihail A. Buryakov <mburyakov@dcn.ftk.spbstu.ru>"
  putStrLn "Press <Enter>."
  _ <- getLine
  return ()

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
  withMem <- return $ checkOpt "--mem" args
  if "--help" `elem` args || args == [] || isNothing cnt || isNothing rows
  then do
    progName <- getProgName
    help progName
  else do
    str <- liftM convertToAscii $ if "--verbose" `elem` args || "all" `elem` args
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
      taskHuffman str
    else do
      return ()
    if "unihuffman" `elem` args || "all" `elem` args
    then do
      taskUniHuffman str
    else do
      return ()
    if "shannon" `elem` args || "all" `elem` args
    then do
      taskShannon str
    else do
      return ()
    if "lzw" `elem` args || "all" `elem` args
    then do
      taskLZW str
    else do
      return ()
    if "testcodes" `elem` args || "all" `elem` args
    then do
      taskTestCodes str
    else do
      return ()
    if "error" `elem` args || "all" `elem` args
    then do
      task4 intsect withMem (fromJust cnt) (fromJust rows) str
    else do
      return ()
    if "--verbose" `elem` args || "all" `elem` args
    then do
      putStrLn "Done. Press <Enter>."
      _ <- getLine
      return ()
    else
      return ()