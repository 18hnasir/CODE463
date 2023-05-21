import Control.Monad        -- many useful functions
import Control.Concurrent   -- threadDelay, forkIO, MVar..., Chan...
import Data.IORef           -- newIORef, readIORef, writeIORef
import System.Environment   -- getArgs

--import System.Random        -- randomRIO, if you attempt a random seating
import Debug.Trace



main :: IO ()
main = do 
  print "HELLO"