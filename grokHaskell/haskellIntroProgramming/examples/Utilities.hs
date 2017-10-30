-- Utilities.hs

-- | A place to hang code snippets.  No attempt to over architecture.

module Utilities where

-- | Return a sorted copy of a list.
--   Illustrating the use of @ for subpattern variable binding.

sort :: Ord a => [a] -> [a]
sort = foldr insert [] where
  insert x [] = [x]
  insert x xs@(y:ys) = if x <= y
    then x : xs
    else y : (insert x ys)

