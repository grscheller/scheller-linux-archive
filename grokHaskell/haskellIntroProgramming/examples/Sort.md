## Lazy evaluate `sort [2,1,3,1]`
Where
```
sort :: Ord a => [a] -> [a]
sort = foldr insert [] where
  insert x [] = [x]
  insert x xs@(y:ys) = if x <= y
    then x : xs
    else y : (insert x ys)
```
and, restricted to lists, `foldr` is defined
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (a:as) = f a (foldr f z as)
```
I won't desugar the list.  Lets force the calculation with `head`.
```
head $ sort [2,1,3,1]
head (sort [2,1,3,1])
head (foldr insert [] [2,1,3,1])
head (insert 2 (foldr insert [] [1,3,1]))
head (insert 2 (insert 1 (foldr insert [] [3,1])))
head (insert 2 (insert 1 (insert 3 (foldr insert [] [1]))))
head (insert 2 (insert 1 (insert 3 (insert 1 (foldr insert [] [])))))
head (insert 2 (insert 1 (insert 3 (insert 1 []))))
head (insert 2 (insert 1 (insert 3 [1])))
head (insert 2 (insert 1 [1, insert 3 []))
head (insert 2 [1, 1, insert 3 []])
head [1, insert 2 [1, insert 3 []]
1
```
