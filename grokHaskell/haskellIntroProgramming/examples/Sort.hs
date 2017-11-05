-- Sort.hs

-- | A place to hang code snippets.  No attempt to over architecture.

module Sort where

-- | Return a sorted copy of a list.
--   Illustrating the use of @ for subpattern variable binding.

sort :: Ord a => [a] -> [a]
sort = foldr insert [] where
  insert x [] = [x]
  insert x xs@(y:ys) = if x <= y
    then x : xs
    else y : (insert x ys)

-- | Return a sorted copy of a list.
--   Exploring laziness in terms of foldr.

sort' :: Ord a => [a] -> [a]
sort' = foldr' insert [] where
  insert x [] = [x]
  insert x xs@(y:ys) = if x <= y
    then x : xs
    else y : (insert x ys)

-- | Impliment fold right for lists.  So I understand how it works.
--   What would make it capable of working with infinite data structures
--   would be an f non-strict in its 2nd argument, this could enable the
--   recursive step to terminate early.

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (a:as) = f a (foldr' f z as)

