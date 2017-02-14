module Lists1(
  myLast, myPenultimate, elementAt, myLength, myReverse, isPalindrome,
  NestedList(Elem, List), flatten, compress, isPalindromeSimple, compressS,
  flattenNCM, pack, myPack, packSpan, rlEncode, compressD
) where

import           Control.Arrow
import           Data.List     (group)

myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

myPenultimate :: [a] -> a
myPenultimate = last . init

elementAt :: [a] -> Integer -> a
elementAt [] _     = error "Empty list"
elementAt xs 0     = head xs
elementAt (_:xs) i = elementAt xs $ i-1

myLength :: [a] -> Integer
myLength []     = 0
myLength [_]    = 1
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs:(myReverse . init $ xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = head xs == last xs && (isPalindrome . init . tail $ xs)

isPalindromeSimple :: Eq a => [a] -> Bool
isPalindromeSimple xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

flattenNCM:: NestedList a -> [a]
flattenNCM (Elem x)      = [x]
flattenNCM (List (x:xs)) = flattenNCM x ++ flattenNCM (List xs)
flattenNCM (List [])     = []

compress:: Eq a => [a] -> [a]
compress = foldr addIfNew []

addIfNew:: Eq a => a -> [a] -> [a]
addIfNew x [] = [x]
addIfNew x l
  | head l == x = l
  | otherwise = x:l

compressS:: Eq a => [a] -> [a]
compressS (x:xs@(y:_))
  | x==y = compressS xs
  | otherwise = x:compressS xs
compressS xs = xs

compressD:: Eq a => [a] -> [a]
compressD []     = []
compressD (x:xs) = x : compressD  (dropWhile (==x) xs)

pack:: Eq a => [a] -> [[a]]
pack = group

myPack:: Eq a => [a] -> [[a]]
myPack []     = []
myPack (x:xs) = (x:takeWhile (==x) xs):myPack(dropWhile (==x) xs)

packSpan:: Eq a => [a] -> [[a]]
packSpan []     = []
packSpan (x:xs) = (x:same):packSpan different
  where (same, different) = span (==x) xs

rlEncode:: Eq a => [a] -> [(Int, a)]
rlEncode = map (length &&& head) . group
