module Huffman where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Bits
import Data.Maybe
import Control.Monad

import Code
import Analisis
import Data.Tuple

mapFst f (a, b) =
  (f a, b)

data DecodeTree a = Symbol a | Node (DecodeTree a) (DecodeTree a)
  deriving (Eq, Ord, Read, Show)

treeDecode (Node t f) (True:ls) =
  treeDecode t ls
treeDecode (Node t f) (False:ls) =
  treeDecode f ls
treeDecode (Symbol c) ls =
  Just (c, ls)
treeDecode _ [] =
  Nothing

data Huffman a = Huffman (Map.Map a [Bool]) (DecodeTree a)

instance StreamCode Huffman where
  tryEncode (Huffman m t) x = Map.lookup x m
  decode (Huffman m t) lst = treeDecode t lst

buildTree s | Set.size s == 1 =
  snd $ Set.findMin s
buildTree s | Set.size s > 1 =
  buildTree $ Set.insert (f1 + f2, Node t1 t2) s''
    where
      ((f1, t1), s') = Set.deleteFindMin s
      ((f2, t2), s'') = Set.deleteFindMin s'

buildTreeFromPhrase str =
  buildTree fr'
    where
      fr' = Set.fromList . map (\(c, f) -> (f, Symbol c)) . Map.toList $ fr
      fr = (freq . count) str

buildTreeFromTable table =
  buildTreeFromTable' $ Map.toList table

buildTreeFromTable' lst =
  case lst of
    [(c ,[])] -> Symbol c
    _         -> Node (buildTreeFromTable' left) (buildTreeFromTable' right)
    where
      left  = [ (c, l) | (c, True  : l) <- lst]
      right = [ (c, l) | (c, False : l) <- lst]

buildTableFromTree tree =
  deserializeTable $ serializeTree tree

buildTableFromTree' (Symbol c) tl =
  Map.singleton c tl
buildTableFromTree' (Node t f) tl =
  Map.union (buildTableFromTree' t (True:tl)) (buildTableFromTree' f (False:tl))

instance Show a => Show (Huffman a) where
  show (Huffman m t) = show t

{--instance (Read a, Ord a) => Read (Huffman a) where
  readsPrec n str = map (\(a,b) -> (Huffman (buildTableFromTree a) a, b)) tree
    where
      tree = readsPrec n str
--}
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

toSelfBinary int =
  toBinary (bitSize int) int

toBinary len int =
  map (testBit int) [0..len-1]

fromBinary lst =
  sum $ map (\(x,n) -> if x then 1 `shiftL` n else 0) $ zip lst [0..]

instance FileCode Huffman where

  build str =
    Huffman canTable canTree
      where
        tree = buildTreeFromPhrase str
        canTable = buildTableFromTree tree
        canTree =  buildTreeFromTable canTable

  serialize (Huffman m t) =
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

    if True then return (Huffman table tree, rest)
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

--maybeAccumWhileCan :: (acc -> x -> Maybe (acc, y)) -> acc -> [Maybe x] -> (acc, [y])
--maybeAccumWhileCan _ s [] = (s, [])
{--maybeAccumWhileCan f s (x:xs) =
  case r of
    Just (s', x') -> x' : (maybeAccumWhileCan f s' xs)
    where
      r = x >>= (\x' -> f s x')
--}

{--
instance Show a => Show (Huffman a) where
  show (Huffman m t) = show t

instance (Read a, Ord a) => Read (Huffman a) where
  readsPrec n str = map (\(a,b) -> (Huffman (buildTableFromTree a) a, b)) tree
    where
      tree = readsPrec n str

canonicalizeTree tree =
  deserializeTree . serializeTree $ tree


--}