module Main where

import           Lists1
main :: IO ()
main =
  print . mylast $ [1,2,3]
