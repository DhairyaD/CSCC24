{-# LANGUAGE RankNTypes #-}
module GameApps where

import Data.Array
import Text.Read

import GameDef
import qualified Game as T

-- Make IO an instance that communicates with the player through stdio.
instance MonadGame IO where
    gmAction = askPlayer

-- Given jug state, ask the player for their reply.  (If invalid input, ask
-- again.)  Return the PlayerMsg representation of their reply.  This function
-- shared by both the IO instance above and guessFreeIO below.
askPlayer :: Array Int Jug -> Goal -> IO PlayerMsg
askPlayer jugs (Goal i x) =
    printJugs jugs
    >> putStrLn ("Goal: Jug #" ++ show i ++ " has " ++ show x ++ "litres.")
    >> putStrLn "Please enter which jug to transfer from:"
    >> getInt "Please enter which jug to transfer from:"
    >>= \src -> getInt "Please enter which jug to transfer to:"
    >>= \tgt -> return (FromTo src tgt)

printJugs :: Array Int Jug -> IO ()
printJugs jugs = mapM_ print1 (assocs jugs)
  where
    print1 (i, Jug cap amt) = putStrLn ("Jug #" ++ show i
                                        ++ " capacity=" ++ show cap
                                        ++ " amount=" ++ show amt)

getInt :: String -> IO Int
getInt msg =
    putStrLn msg
    >> getLine
    >>= \inp ->
    case readMaybe inp of
      Nothing -> putStrLn "Sorry, that's not a number."
                 >> getInt msg
      Just i -> return i

-- Run a given game using the IO instance of MonadGameMaster.
-- (E.g., try "playIO goofy".)
playIO :: (forall m. MonadGame m => m ()) -> IO ()
playIO game = game >>= printEnding

printEnding _ = putStrLn "Finished!"

-- Create and run the jug game using the IO instance of MonadGame.  The
-- parameters are the initial jugs and the goal.
jugIO :: [Jug] -> Goal -> IO ()
jugIO jugs goal = playIO (T.jugGame (mkJugArray jugs) goal)

-- Run a given game using the GameTrace instance of MonadGame, still using
-- stdio.
playTraceIO :: (forall m. MonadGame m => m ()) -> IO ()
playTraceIO game = go game >>= printEnding
  where
    go :: GameTrace () -> IO ()
    go (Pure _) = return ()
    go (Step jugs goal next) = askPlayer jugs goal >>= \req -> go (next req)

-- Create and run the jug game using the GameTrace instance of MonadGame, still
-- using stdio.  The parameters are the initial jugs and the goal.
jugTraceIO :: [Jug] -> Goal -> IO ()
jugTraceIO jugs goal = playTraceIO (T.jugGame (mkJugArray jugs) goal)
