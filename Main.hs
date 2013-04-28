module Main where

import System.Environment
import qualified Data.Map as Map
import Text.Printf
import Data.List

import Analisis
import Code
import Huffman

printTable1 :: (String, String) -> Map.Map Char Float -> IO ()
printTable1 (h1, h2) table = do
  putStrLn                "┌─────┬───────┐"
  printf                  "│%4s │%6s │\n" h1 h2
  putStrLn                "├─────┼───────┤"
  mapM_ (\(c,f) -> printf "│ '%c' │%6.2f │\n" c f) $ Map.toList table
  putStrLn                "└─────┴───────┘"
  putStrLn ""

printTable2 :: (String, String, String) -> [(Int, Float, Float)] -> IO ()
printTable2 (h1, h2, h3) table = do
  putStrLn                    "┌────┬───────┬───────┐"
  printf                      "│%3s │%6s │%6s │\n" h1 h2 h3
  putStrLn                    "├────┼───────┼───────┤"
  mapM_ (\(c,f1,f2) -> printf "│ %2d │%6.2f │%6.2f │\n" c f1 f2) $ table
  putStrLn                    "└────┴───────┴───────┘"
  putStrLn ""

task1 str =
  printTable1 ("x","I(x)") (content str)

task2 str =
  printTable2 ("n","Hn(X)","H(X|.)") (map (\n -> (n, entropyN str n, entropyCondN str n)) [1..10])

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
    putStrLn $ "Compression coefficient: " ++ show ((fromInteger.toInteger) (length str) / (fromInteger.toInteger) (length encoded) * 256)
    where
      (code, encoded) = encodeFile str :: (Huffman Char, [Bool])
      (Just (code', str')) = decodeFile encoded :: Maybe (Huffman Char, [Char])

main = do
  args <- getArgs
  args' <- return $ if args == [] then ["--help"] else args
  if "--help" `elem` args'
  then do
    progName <- getProgName
    putStrLn $ "usage:\n" ++ progName ++ " [--help] [--verbose] [task1] [task2] [task3]\n\n"
      ++ "  --help     print this help\n"
      ++ "  --verbose  read oneline phrase from keyboard (other case read stdin up to eof)\n\n"
      ++ "  task1      information quantity of each symbol of the phrase\n"
      ++ "  task2      entropy of the phrase (contitional and in groups)\n"
      ++ "  task3      encode phrase with universal Huffman code\n"
  else do
    str <- if "--verbose" `elem` args'
    then do
      putStrLn "Enter your text:"
      getLine  
    else do
      getContents
    if "task1" `elem` args'
    then do
      printTable1 ("x","I(x)") (content str)
    else do
      return ()
    if "task2" `elem` args'
    then do
      printTable2 ("n","Hn(X)","H(X|.)") (map (\n -> (n, entropyN str n, entropyCondN str n)) [1..10])
    else do
      return ()
    if "task3" `elem` args'
    then do
      task3 str
    else do
      return ()