module Code where

import Data.Bits
import Data.Char
import Data.Maybe

boolToChar True  = '1'
boolToChar False = '0'

instance Num Char where
  a + b = chr (ord a + ord b)
  a * b = chr (ord a * ord b)
  abs a = chr (abs $ ord a)
  signum a = chr (signum $ ord a)
  fromInteger a = chr (fromInteger a)

instance Bits Char where
  a .&. b = chr (ord a .&. ord b)
  a .|. b = chr (ord a .|. ord b)
  a `xor` b = chr (ord a `xor` ord b)
  complement b = chr (complement $ ord b)
  bitSize _ = 8
  isSigned _ = False
  shift a n = chr (shift (ord a) n)
  rotate a n = chr (rotate (ord a) n)

instance Num Bool where
  a + b = a /= b
  a * b = a && b
  abs a = a
  signum a = if a then 1 else 0
  fromInteger a = a `mod` 2 == 1

instance Bits Bool where
  a .&. b = a && b
  a .|. b = a || b
  a `xor` b = a /= b
  complement b = not b
  bitSize c = 1
  isSigned _ = False
  shift x _ = x
  rotate x _ = x

class StreamCode a where

  canEncode :: Ord b => a b -> b -> Bool
  canEncode c x =
    case tryEncode c x of
      Just _  -> True
      Nothing -> False

  encode :: Ord b => a b -> b -> [Bool]
  encode c x =
    case tryEncode c x of
      Just r -> r
      Nothing -> error $ "Unexpected input symbol"

  tryEncode :: Ord b => a b -> b -> Maybe [Bool]
  tryEncode c x | canEncode c x =
    Just $ encode c x
  
  decode :: Ord b => a b -> [Bool] -> Maybe (b, [Bool])

  encodeStr :: Ord b => a b -> [b] -> [Bool]
  encodeStr coder str =
    concat $ map (encode coder) str

  decodeStr :: Ord b => a b -> [Bool] -> Maybe [b]
  decodeStr coder [] =
    return []
  decodeStr coder lst = do
    (c, lst') <- decode coder lst
    rest <- decodeStr coder lst'
    return $ c : rest


class StreamCode a => FileCode a where

  build :: Ord b => [b] -> a b

  serialize :: (Bits b, Ord b) => a b -> [Bool]

  deserialize :: (Bits b, Ord b) => [Bool] -> Maybe (a b, [Bool])

  encodeFile :: (Bits b, Ord b) => [b] -> (a b, [Bool])
  encodeFile lst =
    (code, serCode ++ encodeStr code lst)
      where
        code = build lst
        serCode = serialize code

  decodeFile :: (Bits b, Ord b) => [Bool] -> Maybe (a b, [b])
  decodeFile lst = do
    (code, lst') <- deserialize lst
    ans <- decodeStr code lst'
    return (code, ans)