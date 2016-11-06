module Main where

import           Lists1
main :: IO ()
main =
  print . elementAt [1,2,3] $ 1
