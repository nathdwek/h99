module Lists2(
  rlmEncode, rlmEncodeNC, rlmDecode, rlDecode, simpleRlDecode
) where

import           Data.List (group)
import           Lists1    (rlEncode)

data RlmItem a = Elem a | RL Int a
  deriving (Show)

rlmEncode:: Eq a => [a] -> [RlmItem a]
rlmEncode = map encodeHelper . rlEncode
  where
    encodeHelper (1, x) = Elem x
    encodeHelper (n, x) = RL n x

rlmEncodeNC:: Eq a => [a] -> [RlmItem a]
rlmEncodeNC = map encodeHelper . group
  where
    encodeHelper [x] = Elem x
    encodeHelper xs  = RL (length xs) (head xs)

rlmDecode:: [RlmItem a] -> [a]
rlmDecode  = concatMap decodeHelper
  where
    decodeHelper(Elem x) = [x]
    decodeHelper(RL l x) = replicate l x

rlDecode:: [(Int, a)] -> [a]
rlDecode = concatMap decodeHelper
  where
    decodeHelper(1,x) = [x]
    decodeHelper(n,x) = x:decodeHelper(n-1,x)

simpleRlDecode:: [(Int, a)] -> [a]
simpleRlDecode = concatMap (uncurry replicate)
