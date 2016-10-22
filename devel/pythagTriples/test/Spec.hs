import PythagTriples

infoMessage :: String
infoMessage = "Just call a function for now. \
              \(test suite not yet implemented):"

main :: IO ()
main = 
  putStrLn ("\n" ++ infoMessage) >> 
  putStrLn (show $ take 20 pythagTriplesFast)
