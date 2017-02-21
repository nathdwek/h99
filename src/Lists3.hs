module Lists3(
  insertAt, range
) where

insertAt:: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y:xs
insertAt n y (x:xs) = x:insertAt (n-1) y xs

range:: Int -> Int -> [Int]
range start end
  |start == end = []
  |otherwise = start:range (start+1) end
