module Turbo where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map

import           TurboDef


-- "Run" a Turbo program to produce SVG path commands.
-- This is the only function that will be tested.
runTurbo :: Stmt -> [SVGPathCmd]
runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
-- But the actual execution engine and recursion are in "turbo" and "evalReal"
-- below.


-- Evaluate an expression. The State monad is needed to look up variables.
evalReal :: RealExpr -> State TurboMem Double
evalReal (RLit literal) = return literal
evalReal (RVar var) = (getVar var)

-- runs evalReal on the expr, then negates it
evalReal (Neg expr) = fmap (negate) (evalReal expr)

-- Recurses on each expression
-- adds, subtracts, multiplies, and divides them and returns the TurboMem Double value
evalReal (expr1 :+ expr2) = (fmap (+) (evalReal expr1)) <*> (evalReal expr2)

evalReal (expr1 :- expr2) = (fmap (-) (evalReal expr1)) <*> (evalReal expr2)

evalReal (expr1 :* expr2) = (fmap (*) (evalReal expr1)) <*> (evalReal expr2)

evalReal (expr1 :/ expr2) = (fmap (/) (evalReal expr1)) <*> (evalReal expr2)

-- Run a Turbo statement. Use the State monad to keep state. Return SVG path
-- commands.
turbo :: Stmt -> State TurboMem [SVGPathCmd]
turbo (var := expr) = do
    val <- evalReal expr
    setVar var val
    return []

turbo PenDown = do
    (setPen True)
    return []

turbo PenUp = do
    (setPen False)
    return []

turbo (Turn expr) = do
    newAngle <- evalReal expr
    turn newAngle
    return []

turbo (Forward expr) = do
    penState <- getPen
    val <- evalReal expr
    myAngle <- getAngle
    if penState == True -- if True then pen is on paper, otherwise just move
        -- multiplied by pi/180 because trig functions take radians
        then return [(LineTo ((cos(myAngle * (pi/180))) * val) ((sin(myAngle * (pi/180))) * val))]
        else return [(MoveTo ((cos(myAngle * (pi/180))) * val) ((sin(myAngle * (pi/180))) * val))]

turbo (Seq (x:xs)) = do
    cmnd <- turbo x -- recurses on just first element on list with turbo
    if xs == []
        then return cmnd
        else do
            allCmnds <- turbo (Seq xs)
            return (cmnd ++ allCmnds) -- recurses on the rest of the list

turbo (For loop expr1 expr2 myStmt) = do
    min <- evalReal(expr1)
    max <- evalReal(expr2)
    cmnd <- turbo (Seq myStmt) -- run turbo once on the list of statements
    setVar loop min
    if (min < max)
        then do
            allCmnds <- turbo (For loop (RLit(min+1)) expr2 myStmt) -- recurses and increments counter by 1
            return (cmnd ++ allCmnds)
        else return cmnd

-- Turbo state:
-- * dictionary of variables->values
-- * current direction (degrees away from x-axis, counterclockwise, e.g.,
--   0 points west, 90 points north)
-- * pen state (True means touching paper)
data TurboMem = TurboMem (Map String Double) Double Bool
    deriving (Eq, Show)

-- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
initTurboMem = TurboMem Map.empty 0 False

-- If you could code up the following helpers, your "turbo" implementation could
-- be pretty clean and easy to follow.  fmap, get, modify, Map.lookup, and
-- Map.insert will get you a long way.

-- Get current direction.
getAngle :: State TurboMem Double
getAngle = do
    TurboMem _ dir _ <- get
    return dir

-- Change direction by adding the given angle.
turn :: Double -> State TurboMem ()
turn newDir = do
    modify (\(TurboMem varMap dir penState) -> (TurboMem varMap (dir + newDir) penState))

-- Get pen state.
getPen :: State TurboMem Bool
getPen = do
    TurboMem _ _ penState <- get
    return penState

-- Set pen state.
setPen :: Bool -> State TurboMem ()
setPen newPenState = do
    modify (\(TurboMem varMap dir penState) -> (TurboMem varMap dir newPenState))

-- Get a variable's current value.
getVar :: String -> State TurboMem Double
getVar var = do
    TurboMem varMap _ _ <- get
    return (Map.findWithDefault 0 var varMap)

-- Set a variable to value.
setVar :: String -> Double -> State TurboMem ()
setVar key val = do
    modify (\(TurboMem varMap dir penState) -> (TurboMem (Map.insert key val varMap) dir penState))
