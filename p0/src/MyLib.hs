module MyLib where

import Data.Maybe

zipLong :: [a] -> [b] -> [(a, b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong xs ys = take maxLength $ zipWith (,) (cycle xs) (cycle ys)
  where maxLength = max (length xs) (length ys)


