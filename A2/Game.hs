module Game where

import Control.Monad
import Data.Array

import GameDef


-- Question 1.

-- The game master for the jug game.  The parameters are the initial jug state
-- and the goal.
jugGame :: MonadGame m => Array Int Jug -> Goal -> m ()
jugGame jugsArray (Goal goalJug goalAmt)
    | currAmt == goalAmt = return()
    | otherwise = do
        msg <- gmAction (jugsArray) (Goal goalJug goalAmt)
        jugGame (moveJugs msg jugsArray (Goal goalJug goalAmt)) (Goal goalJug goalAmt)
    where
        (Jug capacity currAmt) = jugsArray ! goalJug

-- This helper function moves the water from source jug to the target jug
-- first guard checks indexes out of range, if so, return back to jugGame
-- the 2nd and 3rd guards check whether the source is empty or the target is full
-- which if they are, then they go back to jugGame and requests from the user again
-- Recursive case in otherwise is to update jugs and keep going until one is empty or other is full
moveJugs :: PlayerMsg -> Array Int Jug -> Goal -> Array Int Jug
moveJugs (FromTo src tgt) jugsArray (Goal goalJug goalAmt)
    | (src >= (length jugsArray)) || (tgt >= (length jugsArray)) = jugsArray
    | srcAmt == 0 =  jugsArray
    | tgtCapacity == tgtAmt = jugsArray
    | otherwise = moveJugs (FromTo src tgt) newJugsArray (Goal goalJug goalAmt)
    where
        -- gets the source jug and the target jug first
        -- then takes the old array of jugs and updates the source and target jug
        (Jug srcCapacity srcAmt) = jugsArray ! src
        (Jug tgtCapacity tgtAmt) = jugsArray ! tgt
        newJugsArray = jugsArray // [(src, (Jug srcCapacity (srcAmt-1))), (tgt, (Jug tgtCapacity (tgtAmt+1)))]


-- Question 2.

instance Functor GameTrace where
    -- If you are confident with your Monad instance, you can just write
    -- fmap :: (a -> b) -> GameTrace a -> GameTrace b
    fmap = liftM

instance Applicative GameTrace where
    -- If you are confident with your Monad instance, you can just write
    -- pure :: a -> GameTrace a
    pure = return

    -- If you are confident with your Monad instance, you can just write
    -- (<*>) :: GameTrace (a -> b) -> GameTrace a -> GameTrace b
    (<*>) = ap

instance Monad GameTrace where
  -- return :: a -> GameTrace a
   return a = Pure a

   -- (>>=) :: GameTrace a -> (a -> GameTrace b) -> (GameTrace b)
   (Pure a) >>= p = p a
   Step jugsArray (Goal goalJug goalAmt) fnc >>= p = Step jugsArray (Goal goalJug goalAmt) (\i -> (fnc i >>= p))



instance MonadGame GameTrace where
    -- gmAction :: Array Int Jug -> Goal -> GameTrace PlayerMsg
    gmAction jugsArray (Goal goalJug goalAmt) = Step jugsArray (Goal goalJug goalAmt) (\i -> Pure i)


-- Question 3.

-- solution version:
testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testOneStep g =
    chkStep start jugs0 goal
    >>= \next ->
       chkStep (next (FromTo 0 1)) [Jug 7 3, Jug 4 4, Jug 5 2] goal
    >> chkStep (next (FromTo 0 2)) [Jug 7 2, Jug 4 2, Jug 5 5] goal
    >> chkStep (next (FromTo 1 0)) [Jug 7 7, Jug 4 0, Jug 5 2] goal
    >> chkStep (next (FromTo 1 2)) [Jug 7 5, Jug 4 0, Jug 5 4] goal
    >> chkStep (next (FromTo 2 0)) [Jug 7 7, Jug 4 2, Jug 5 0] goal
    >> chkStep (next (FromTo 2 1)) [Jug 7 5, Jug 4 4, Jug 5 0] goal
    >> return ()
  where
    goal = Goal 2 1
    jugs0 = [Jug 7 5, Jug 4 2, Jug 5 2]
    start = g (mkJugArray jugs0) goal

testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testUntilDone g =
    chkStep start jugs0 goal
    >>= \next1 -> chkStep (next1 (FromTo 2 1)) [Jug 2 0, Jug 3 3, Jug 4 1] goal
    >>= \next2 -> chkPure (next2 (FromTo 2 0)) ()
  where
    goal = Goal 0 1
    jugs0 = [Jug 2 0, Jug 3 0, Jug 4 4]
    start = g (mkJugArray jugs0) goal

-- Helpers!

chkPure (Pure a1) a2 | a1 == a2 = Just ()
chkPure _ _ = Nothing

chkStep (Step j1 g1 next) j2 g2
    | j1 == mkJugArray j2 && g1 == g2 = Just next
chkStep _ _ _ = Nothing
