module Lists1(
  myLast, myButLast, elementAt, myLength, myReverse, isPalindrome,
  NestedList(Elem, List), flatten
) where

myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast = last . init

elementAt :: [a] -> Integer -> a
elementAt [] i     = error "Empty list"
elementAt xs 0     = head xs
elementAt (x:xs) i = elementAt xs $ i-1

myLength :: [a] -> Integer
myLength []     = 0
myLength [x]    = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs:(myReverse . init $ xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs  = head xs == last xs && (isPalindrome . init . tail $ xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
