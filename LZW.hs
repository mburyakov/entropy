module LZW where

import qualified Data.Map as Map
import Data.List

import Code

data LZWOptions = LZWFixedLength Int | LZWGrowing | LZWBounded Int

bitS 0 = 0
bitS n =
  1 + bitS (n `div` 2)

lzwBitSize (LZWFixedLength l) n = l
lzwBitSize LZWGrowing         n = bitS (n-1)
lzwBitSize (LZWBounded l)     n = min (bitS (n-1)) l

data LZW a = LZW {options :: LZWOptions, counter :: Int, dictionary :: Map.Map [a] Int, reverseDict :: Map.Map Int [a], buffer :: [a], lastBuffer :: [a]}

buildLZW :: (Bounded a, Enum a, Ord a) => LZWOptions -> LZW a
buildLZW opts =
  LZW opts n (Map.fromList dict) (Map.fromList revDict) [] []
    where
      lst  = map (:[]) $ enumFromTo minBound maxBound
      dict = zip lst [0..]
      revDict = zip [0..] lst
      n = length lst

addLZW next code@(LZW opts n dict revDict buf _) =
  LZW opts (n+1) (Map.insert (next: reverse buf) n dict) (Map.insert n (next:reverse buf) revDict) [next] buf

stepLZW next (LZW opts n dict revDict buf lastBuf) =
  LZW opts n dict revDict (next:buf) lastBuf

instance StreamCode LZW where
  canEncode = const $ const True
  encode code@(LZW opts n dict revDict buf lastBuf) next =
    case Map.lookup (next:buf) dict of
      Just n  -> (stepLZW next code, [])
      Nothing -> (addLZW next code, toBinary (lzwBitSize opts n) bufNum)
        where
          (Just bufNum) = Map.lookup buf dict
  decode code@(LZW opts n dict revDict buf lastBuf) lst =
    case buf of
      [] -> do
        word <- if bufNum==n then Just $ last lastBuf : lastBuf else Map.lookup bufNum revDict
        (LZW _ n' dict' revDict' _ _) <- return $ case lastBuf of []->code; _->addLZW (last word) (LZW opts n dict revDict lastBuf lastBuf)
        return ((LZW opts n' dict' revDict' (tail $ reverse word) word), (last word, drop bits lst))
      x:xs  -> Just (LZW opts n dict revDict xs lastBuf, (x, lst))
      where
        bufNum = fromBinary $ take bits lst
        bits = lzwBitSize opts $ case lastBuf of []->n; _->(n+1)
  encodeStr coder str =
    concat (snd ans) ++ snd pushBuf
      where
        ans = mapAccumL encode coder str
        (LZW opts n dict revDict buf lastBuf) = fst ans
        pushBuf = encode (LZW opts n (Map.delete (head buf:buf) dict) revDict buf lastBuf) (head buf)
  decodeStr coder [] =
    return $ reverse $ buffer coder
  decodeStr coder lst = do
    (coder', (c, lst')) <- decode coder lst
    rest <- decodeStr coder' lst'
    return (c:rest)

instance Buildable LZW where
  build str =
    buildLZW (LZWGrowing)