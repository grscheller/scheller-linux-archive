import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- forkIO (replicateM_ 20 (putChar 'A'))
  replicateM_ 20 (putChar 'B')
