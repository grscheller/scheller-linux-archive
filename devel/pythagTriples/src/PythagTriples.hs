module PythagTriples
  ( printTriples
  , pythagTriplesOrdered
  , pythagTriplesFast
  , sortTriple
  , Triple
  ) where

type Triple = (Int, Int, Int)

sortTriple :: Triple -> Triple
sortTriple triple =
  case triple of
    (a, b, c) | b < a -> (b, a, c)
    _                 -> triple

printTriples :: [Triple] -> IO()
printTriples triples = mapM_ putStrLn (map show triples)

pythagTriplesFast :: [Triple]
pythagTriplesFast = [ (a, 2*m*n, c) |
    m <- [2 ..]
  , let nstart = m `mod` 2 + 1
  , n <- [nstart, nstart+2 .. m-1]
  , let a = m*m - n*n
  , let c = m*m + n*n
  , gcd a c == 1 ]

pythagTriplesOrdered :: [Triple]
pythagTriplesOrdered = [ (a, b, floor.sqrt $ fromIntegral csq) |
      a <- [3 .. ]
    , b <- [a+1, a+3 .. ((a*a - 1) `div` 2)]
    , gcd b a == 1
    , let csq = a*a + b*b
    , isPerfectSquare csq ]
  where
    isPerfectSquare :: Int -> Bool
    isPerfectSquare n = n == (floor.sqrt $ fromIntegral n)^2
