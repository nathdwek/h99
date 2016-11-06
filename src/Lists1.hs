module Lists1(myLast, myButLast, elementAt) where

myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast []     = error "Empty list"
myButLast [x]    = error "List has only one element"
myButLast [x,y]  = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Integer -> a
elementAt [] i     = error "Empty list"
elementAt xs 0     = head xs
elementAt (x:xs) i = elementAt xs $ i-1
