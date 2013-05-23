module Huffman where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Bits
import Data.Maybe
import Data.Tuple
import Control.Monad

import Code
import Analisis

buildTreeFromPhrase str =
  buildTree fr'
    where
      fr' = Set.fromList . map (\(c, f) -> (f, Symbol c)) . Map.toList $ fr
      fr = (freq . count) str


data Huffman a = Huffman (TreeCode a)

instance StreamCode Huffman where
  decode code@(Huffman h) str = decode h str >>= (\x -> return (code, snd x))
  encode code@(Huffman h) str = (code, snd $ encode h str)

instance Serializable Huffman where
  serialize (Huffman (TreeCode m t)) =
    if True then lensbits ++ chars
    else error $ show $ take 20 $ map (uncurry toBinary) $ zip [1..] lensquant
      where
        chlens = serializeTree t
        lens = map fst chlens
        lensquant = map (length . \x -> filter (==x) lens) [0..]
        lensbits = concat $ take (acclen (length lens) lensquant) $ map (uncurry toBinary) $ zip [1..] lensquant
        chars = concat $ map (toSelfBinary . snd) chlens
        acclen 0 _ = 0
        acclen n (f:lst) =
          1 + acclen (n-f) lst

  deserialize lst = do
    assertion1 <- if length (take (len+1) gap) > len && (gap !! len == 0) then Just () else Nothing
    lens <- sequence $ take (len+1) lens'
    space <- return $ sum lens
    ch' <- return $ drop binlen lst
    charsch <- return $ take space $ chars ch'
    assertion2 <- if length charsch == space then Just () else Nothing
    lens'' <- return $ concat $ map (\(l, q) -> replicate q l) $ zip [0..] lens
    table <- return $ deserializeTable $ zip (reverse lens'') charsch
    tree <- return $ buildTreeFromTable table
    rest <- return $ drop (binlen + space * bitSize (head charsch)) lst

    if True then return (Huffman $ TreeCode table tree, rest)
    else error $ show $ groupList 1 ch'
      where
        lens' = readlst 1 lst
        gap = snd $ mapAccumL (\acc x -> (2*(acc-x), acc-x)) (1::Int) $ takeJust lens'
        len = (length . takeWhile (>0)) gap
        binlen = (len + 2) * (len + 1) `div` 2
        
        chars x = takeJust $ map (liftM fromBinary) (groupList (bitSize $ head $ chars x) x)
        
        takeJust (Just x : xs) =
          x : takeJust xs
        takeJust _ =
          []
        calcGap acc (Just x) =
          Just (2*(acc-x), acc-x)
        calcGap acc Nothing =
          Nothing


        tryTake n lst | length lst >= n =
          Just $ take n lst
        tryTake n lst =
          Nothing

        groupList n [] =
          []
        groupList n lst = 
          tryTake n lst : groupList n (drop n lst)

        readlst n lst =
          liftM fromBinary (tryTake n lst) : readlst (n+1) (drop n lst)


instance Buildable Huffman where
  build str =
    Huffman $ TreeCode canTable canTree
      where
        tree = buildTreeFromPhrase str
        canTable = buildTableFromTree tree
        canTree =  buildTreeFromTable canTable

instance FileCode Huffman