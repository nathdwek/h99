module Lists2(
  rlmEncode
) where

import           Lists1 (rlEncode)

data RlmItem a = Single a | Multiple Int a
  deriving (Show)

rlmEncode:: Eq a => [a] -> [RlmItem a]
rlmEncode = map encodeHelper . rlEncode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x
