module Main where

import           Lists1

main :: IO ()
main =
  print . flatten $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
