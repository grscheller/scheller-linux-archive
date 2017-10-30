-- ListFunctions.hs (Lecture 2)

-- | Functions for manipulating lists

module ListFunctions where

-- | Count the number of elements in a list.
--   Type I think the standard library should have choosen for length.

length' :: Num n => [a] -> n
length' [] = 0
length' (_:cs) = 1 + length' cs

-- | Compute the sum of the numbers in a list.

sum' :: Num n => [n] -> n
sum' [] = 0
sum' (x:xs) = x + sum xs

-- | Compute the sum of the squares of the numbers in a list.
--   Naively repeating the length' and sum' pattern.

sumSquares :: Num n => [n] -> n
sumSquares [] = 0
sumSquares (x:xs) = x*x + sumSquares xs

-- | Given a function f, compute the sum of the (f x) for each x in xs.

sumf :: Num n => (a -> n) -> [a] -> n
sumf f [] = 0
sumf f (c:cs) = f c + sumf f cs

-- | Square of a number

square :: Num n => n -> n
square x = x * x

-- | Cube of a number

cube :: Num n => n -> n
cube x = x * x * x

-- | Compute the sum of the squares of the numbers in a list.
--   Note the η-reduction.

sumSquares' :: Num n => [n] -> n
sumSquares' = sumf square 

-- | Compute the sum of the cubes of the numbers in a list.
--   As with sumSquares', we have a "separation of concerns" in the
--   sense that the recursive summing part and function computing
--   part have been unconflated and more composable.

sumCubes' :: Num n => [n] -> n
sumCubes' = sumf cube 

-- | Compute the sum of the numbers in a list.

sum'' :: Num n => [n] -> n
sum'' = sumf id

-- | Map a function accross a list.

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

-- | Compute the sum of the cubes of the numbers in a list.

sumSquares'' :: Num n => [n] -> n
sumSquares'' xs = sum (map' square xs)

-- | Compute the sum of the cubes of the numbers in a list.

sumSquares''' :: Num n => [n] -> n
sumSquares''' xs = sum $ map' square xs

-- | Compute the sum of the cubes of the numbers in a list.

sumSquares'''' :: Num n => [n] -> n
sumSquares'''' = sum . map' square

-- | Count the number of elements in a list.
--   Using a left fold and a λ-function.  

length'' :: Num n => [a] -> n
length'' = foldl (\n _ -> n + 1) 0

-- | Count the number of elements in any foldable type.
--   Using a left fold and a λ-function.  

length''' :: (Foldable t, Num n) => t a -> n
length''' = foldl (\n _ -> n + 1) 0

-- | Count the number of elements in any foldable functor.
--   No, I don't really know what I am doing.

length'''' :: (Functor t, Foldable t, Num n) => t a -> n
length'''' = (foldl (+) 0) . ((<$) 1) 

-- | Given a function f, compute the product of the (f x) for each x in xs.

productf :: Num n => (a -> n) -> [a] -> n
productf f [] = 1
productf f (c:cs) = f c * productf f cs

-- | Compute the product of the numbers in a list.

product' :: Num n => [n] -> n
product' = productf id

-- | Compute the product of the squares of numbers in a list.

productSquares :: Num n => [n] -> n
productSquares = productf square

-- | Compute the sum of squares of the odd integers in a list.
--   Introducing pattern guards.  Unlike patterns, guards cannot
--   introduce new bindings.  Each guard is considered in order
--   until one evaluates to True.  Also, otherwise == True.

sumSquaresOfOdds :: Integral n => [n] -> n
sumSquaresOfOdds [] = 0
sumSquaresOfOdds (x:xs)
    | odd x     = x*x + sumSquaresOfOdds xs
    | otherwise = sumSquaresOfOdds xs

-- | Return a sublist comprised of the elements of a list that
--   satisfies a predicate.

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter' p xs

-- | Compute the sum of squares of the odd integers in a list.

sumSquaresOfOdds' :: Integral n => [n] -> n
sumSquaresOfOdds' = sum'' . map' square . filter' odd
