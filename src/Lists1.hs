module Lists1(mylast) where

mylast :: [a] -> a
mylast []     = error "Empty list"
mylast [x]    = x
mylast (x:xs) = mylast xs
