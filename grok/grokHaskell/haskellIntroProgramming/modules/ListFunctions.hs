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

-- Exercise 2.3
-- Define divisibleBy, allp, filterAll

-- | Determine if 1st arg evenly divides 2nd arg.
--   Partially apply to produce a "divide by' predicate.

divisibleBy :: Integral m => m -> m -> Bool
divisibleBy d n = n `rem` d == 0

-- | Apply a predicate across a list, return true if true for all elements.
--   Stumbled on this trying to initially define allp without recursion.

all' :: (a -> Bool) -> [a] -> Bool
all' = \x -> foldr ((&&) . ($) x) True

-- | Create a predicate satisfying all predicates in a list.
--   Gave up and used recursion.

allp :: [a -> Bool] -> a -> Bool
allp [] _     = True
allp (p:ps) a = p a && allp ps a

-- | Filter with all predicates in a list.

filterAll = filter . allp

-- Exercise 2.4
-- Define allp without recursion using Prelude function and

-- | Create a predicate satisfying all predicates in a list.

allp' :: [a -> Bool] -> a -> Bool
allp' ps = 
  let
    f x = map (flip ($) x)
  in
    and . (flip f) ps

-- | Filter with all predicates in a list.

filterAll' = filter . allp'

-- Apply examples given in lecture notes to above:

main :: IO ()
main =  putStrLn "These next 4 values should be equal:"
     >> putStrLn (show result0)
     >> putStrLn (show result1)
     >> putStrLn (show result2)
     >> putStrLn (show result3)

result0 = sum
        . take 100
        . filter (divisibleBy 2)
        . filter (divisibleBy 3)
        . filter (not . divisibleBy 4)
        . filter (not . divisibleBy 9)
        $ [0..]

result1 = sum
        . take 100
        . filter (not . divisibleBy 9)
        . filter (not . divisibleBy 4)
        . filter (divisibleBy 3)
        . filter (divisibleBy 2)
        $ [0..]

result2 = sum
        . take 100
        . filterAll [ divisibleBy 2
                    , divisibleBy 3
                    , not . divisibleBy 4
                    , not . divisibleBy 9
                    ]
        $ [0..]

result3 = sum
        . take 100
        . filterAll' [ divisibleBy 2
                     , divisibleBy 3
                     , not . divisibleBy 4
                     , not . divisibleBy 9
                     ]
        $ [0..]

