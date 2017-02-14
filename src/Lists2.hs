module Lists2(
  rlmEncode, rlmEncodeNC, rlmDecode, rlDecode, simpleRlDecode,
  directRlEncode, directRlmEncode, dupli, repli, repliF, repliCM, myDrop
) where

import           Control.Arrow
import           Data.List     (group)
import           Lists1        (rlEncode)

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

directRlEncode:: Eq a => [a] -> [(Int, a)]
directRlEncode (x:xs) = (1+length same, x) : directRlEncode different
  where
    (same, different) = span (==x) xs
directRlEncode [] = []

directRlmEncode:: Eq a => [a] -> [RlmItem a]
directRlmEncode (x:xs@(y:_))
  | x == y = RL (1 + length same) x : directRlmEncode different
  | otherwise = Elem x : directRlmEncode xs
    where
      (same, different) = span (==x) xs
directRlmEncode [x] = [Elem x]
directRlmEncode [] = []

dupli:: [a] -> [a]
dupli = foldr (\ x -> (++) [x, x]) []

repli:: Int -> [a] -> [a]
repli n (x:xs) = replicate n x ++ repli n xs
repli n []     = []

repliF:: Int -> [a] -> [a]
repliF n = foldr ((++) . replicate n) []

repliCM:: Int -> [a] -> [a]
repliCM n = concatMap (replicate n)

myDrop:: [a] -> Int -> [a]
myDrop xs n = dropHelper xs n n
  where dropHelper [] _ _         = []
        dropHelper (x:xs) 1 reset = dropHelper xs reset reset
        dropHelper (x:xs) n reset = x : dropHelper xs (n - 1) reset

split:: Int -> [a] -> [[a]]
split 1 x:xs
split n x:xs = x:split (n-1) xs
