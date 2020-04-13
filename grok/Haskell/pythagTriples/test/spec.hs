import Data.List
import PythagTriples

main :: IO ()
main =
  test1 5 >> test1 10 >> test1 20 >> test1 50 >>
  test2 10 >> test2 100 >> test2 1000 >> test2 10000 >> test2 100000 

test1 :: Int -> IO ()
test1 nn =
    putStrLn ("\nTest1: Compare " ++ show nn ++ " (Ordered, Sorted Fast)") >>
    mapM_ (putStrLn.show) (zip py3Ordered py3SortedFast)
  where
    py3Ordered = take nn pythagTriplesOrdered1
    py3SortedFast = sortTriples $ take nn pythagTriplesFast


test2 :: Int -> IO ()
test2 nn =
    (putStrLn ("\nTest2: " ++ show nn)) >> 
    mapM_ (putStrLn.show) (zip py3Ordered py3SortedFast)
  where
    ordered    = take nn pythagTriplesOrdered1
    sortedFast = sortTriples $ take nn pythagTriplesFast
    lastEq     = lastNotEq ordered sortedFast
    py3Ordered    = take 5 $ drop (lastEq - 1) ordered
    py3SortedFast = take 5 $ drop (lastEq - 1) sortedFast

sortTriples :: [Triple] -> [Triple]
sortTriples triples = sort $ map sortTriple triples

lastNotEq :: [(Triple)] -> [(Triple)] -> Int
lastNotEq trip1 trip2 = 
    iterIt 0 trip1 trip2
  where
    iterIt :: Int -> [Triple] -> [Triple] -> Int
    iterIt n [] trip2 = n
    iterIt n trip1 [] = n
    iterIt n trip1 trip2 =
      if head trip1 == head trip2
        then iterIt (n+1) (tail trip1) (tail trip2)
        else n
