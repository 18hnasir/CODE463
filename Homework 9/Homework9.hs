{-
Your Name: Hammadullah Nasir G#01112406
Partner: none
-}

module Homework9 where

import Control.Monad        -- many useful functions
import Control.Concurrent   -- threadDelay, forkIO, MVar..., Chan...
import Data.IORef           -- newIORef, readIORef, writeIORef
import System.Environment   -- getArgs

--import System.Random        -- randomRIO, if you attempt a random seating
import Debug.Trace

{-
-- download BoundedChan from hackage if you want to use this one.
-- You'll get: BC.BoundedChan, BC.newBoundedChan, BC.readChan, BC.writeChan, etc.

-- import qualified Control.Concurrent.BoundedChan as BC
-}

data Chair a = Snoc a 

announcer :: Int -> MVar String -> IO ()
announcer 0 _ = return ()
announcer n mvar = do
  v <- takeMVar mvar
  putStrLn  v
  announcer (n-1) mvar

play :: Int -> MVar String -> IO () --PID OutputMVar 
play pid outputs = do
  putMVar outputs $ "Player " ++ show pid ++ " here"
  return ()

--Creates n player threads  
createPlayers :: Int -> Int -> MVar String -> IO ThreadId -- 1 n 
createPlayers pid n outputs | pid == n = forkIO (play pid outputs) 
                         | otherwise = do 
                                        forkIO (play pid outputs) 
                                        createPlayers (pid + 1) n outputs  

--Creates n - 1 chairs
--createChairs :: Int -> Int -> [MVar Chair] --chairNum n 
--createChairs cn n = [] 


main :: IO ()
main = do
  let n = 5 --default number of players in the game 
  outputs <- newEmptyMVar --the outputs of the program, Ex. "Music off", "P1 lost" etc. 

  createPlayers 1 n outputs
  announcer 5 outputs 

  putStrLn "DONE HERE"