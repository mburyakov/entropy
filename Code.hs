module Code where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Bits
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

mapFst f (a, b) =
  (f a, b)

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

class Enum' a where
  minBound' :: a
  maxBound' :: a

instance Enum' Char where
  minBound' = '\NUL'
  maxBound' = '\255'

class StreamCode a where

  canEncode :: Ord b => a b -> b -> Bool
  canEncode c x =
    case tryEncode c x of
      Just _  -> True
      Nothing -> False

  encode :: Ord b => a b -> b -> (a b, [Bool])
  encode c x =
    case tryEncode c x of
      Just r -> r
      Nothing -> error $ "Unexpected input symbol"

  tryEncode :: Ord b => a b -> b -> Maybe (a b, [Bool])
  tryEncode c x | canEncode c x =
    Just $ encode c x
  tryEncode c x =
    Nothing
  
  decode :: (Show b, Ord b) => a b -> [Bool] -> Maybe (a b, (b, [Bool]))

  encodeStr :: Ord b => a b -> [b] -> [Bool]
  encodeStr coder str =
    concat $ snd ans
      where
        ans = mapAccumL encode coder str

  decodeStr :: (Show b, Ord b) => a b -> [Bool] -> Maybe [b]
  decodeStr coder [] =
    return []
  decodeStr coder lst = do
    (coder', (c, lst')) <- decode coder lst
    rest <- decodeStr coder' lst'
    return (c:rest)

class Serializable a where
  serialize :: (Bits b, Ord b) => a b -> [Bool]
  deserialize :: (Bits b, Ord b) => [Bool] -> Maybe (a b, [Bool])

class Buildable a where
  build :: (Bounded b, Enum b, Enum' b, Ord b) => [b] -> a b

class (StreamCode a, Serializable a, Buildable a) => FileCode a where
  encodeFile :: (Bits b, Enum b, Enum' b, Bounded b, Ord b) => [b] -> (a b, [Bool])
  encodeFile lst =
    (code, serCode ++ encodedStr)
      where
        code = build lst
        serCode = serialize code
        encodedStr = encodeStr code lst

  decodeFile :: (Show b, Bits b, Ord b) => [Bool] -> Maybe (a b, [b])
  decodeFile lst = do
    (code, lst') <- deserialize lst
    ans <- decodeStr code lst'
    return (code, ans)


data DecodeTree a = Symbol a | Node (DecodeTree a) (DecodeTree a) | EmptyNode
  deriving (Eq, Ord, Read, Show)

treeDecode (Node t f) (True:ls) =
  treeDecode t ls
treeDecode (Node t f) (False:ls) =
  treeDecode f ls
treeDecode (Symbol c) ls =
  Just (c, ls)
treeDecode t ls =
  Nothing

data TreeCode a = TreeCode (Map.Map a [Bool]) (DecodeTree a)

instance Show a => Show (TreeCode a) where
  show (TreeCode m t) = show t

instance StreamCode TreeCode where
  tryEncode code@(TreeCode m t) x   = Map.lookup x m   >>= (\x -> return (code, x))
  decode    code@(TreeCode m t) lst = treeDecode t lst >>= (\x -> return (code, x))

toSelfBinary int =
  toBinary (bitSize int) int

toInfBinary 0 =
  cycle [False]
toInfBinary n =
  (n `mod` 2 == 1) : toInfBinary (n `div` 2)

toBinary len int =
  map (testBit int) [0..len-1]

fromBinary lst =
  sum $ map (\(x,n) -> if x then 1 `shiftL` n else 0) $ zip lst [0..]

buildTree s | Set.size s == 1 =
  snd $ Set.findMin s
buildTree s | Set.size s > 1 =
  buildTree $ Set.insert (f1 + f2, Node t1 t2) s''
    where
      ((f1, t1), s') = Set.deleteFindMin s
      ((f2, t2), s'') = Set.deleteFindMin s'

buildTreeFromTable table =
  buildTreeFromTable' $ Map.toList table

buildTreeFromTable' lst =
  case lst of
    []        -> EmptyNode
    [(c ,[])] -> Symbol c
    _         -> Node (buildTreeFromTable' left) (buildTreeFromTable' right)
    where
      left  = [ (c, l) | (c, True  : l) <- lst]
      right = [ (c, l) | (c, False : l) <- lst]

serializeTree tree =
  sortBy (flip compare) $ map swap $ Map.toList $ Map.map length $ Map.map reverse $ buildTableFromTree' tree []

binCount n [] =
  replicate n False
binCount n ls =
  tr
    where
      tr =
        if n<=0
        then
          inc $ nip (-n) ls
        else
          replicate n False ++ inc ls
      nip 0 ls = ls
      nip n (True:ls) | n>0 =
        nip (n-1) ls
      inc (False:ls') =
        True:ls'
      inc (True:ls') =
        False:inc ls'

deserializeTable lengths =
  Map.fromList $ map swap $ snd table'
    where
      table' = mapAccumR f (0, []) lengths
      f (l', ls') (l, c) = ((l, ls), (reverse ls, c))
        where
          ls = binCount (l-l') ls'

buildTableFromTree tree =
  deserializeTable $ serializeTree tree

buildTableFromTree' (Symbol c) tl =
  Map.singleton c tl
buildTableFromTree' (Node t f) tl =
  Map.union (buildTableFromTree' t (True:tl)) (buildTableFromTree' f (False:tl))
buildTableFromTree' EmptyNode tl =
  Map.empty