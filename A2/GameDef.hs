module GameDef where

import Data.Array

-- Jug capacity (1st field) and current amount (2nd field).
data Jug = Jug Integer Integer
    deriving (Eq, Show)

-- Game functions mostly work with Array Int Jug, but it's more convenient for
-- users and testers to give [Jug].  This function helps with the conversion.
mkJugArray :: [Jug] -> Array Int Jug
mkJugArray jugs = listArray (0, length jugs - 1) jugs

-- Goal to be achieved: jug #i (1st field) must have exactly x amount (2nd
-- field).
data Goal = Goal Int Integer
    deriving (Eq, Show)

-- Player's reply: Transfer from source jug (1st field) to target jug (2nd
-- field).
data PlayerMsg = FromTo Int Int
    deriving (Eq, Show)

-- The MonadGame class contains the extra operation a game master needs.
class Monad m => MonadGame m where
    -- This is how the game master communicates with the player.  The game
    -- master calls this function to send the current jug state to the player,
    -- then receive the player's reply in the return value.
    gmAction :: Array Int Jug -> Goal -> m PlayerMsg

-- You will make this type an instance of MonadGameMaster (and Monad etc).
data GameTrace a
    = Pure a
    | Step (Array Int Jug) Goal (PlayerMsg -> GameTrace a)

instance Show a => Show (GameTrace a) where
    showsPrec d (Pure a) =
        showParen (d > 10) (showString "Pure " . showsPrec 11 a)
    showsPrec d (Step jugs goal next) =
        showParen (d > 10) (showString "Step "
                            . shows jugs . showChar ' '
                            . shows goal
                            . showString " (function)")
