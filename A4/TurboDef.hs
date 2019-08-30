module TurboDef where

-- Arithmetic expressions with real number (Double) operands.
data RealExpr
    = RLit Double               -- literal/constant
    | RVar String               -- read var's current value
                                -- if uninitialized, the answer is 0
    | Neg RealExpr              -- unary minus
    | RealExpr :+ RealExpr      -- plus
    | RealExpr :- RealExpr      -- minus
    | RealExpr :* RealExpr      -- times
    | RealExpr :/ RealExpr      -- divide
    deriving (Eq, Ord, Read, Show)

-- "RealExpr :+ RealExpr" is like "Plus RealExpr RealExpr" except I don't want
-- the data constructor to be a prefix, I want it infix and look/used like an
-- infix operator.  The data constructor here is called ":+".  You can now write
-- like "RLit 0 :+ RLit 0.5".  When pattern matching you just code like
--
--     f (r1 :+ r2) = ...
--
-- (As opposed to "f (Plus r1 r2) = ..".)
--
-- And the following sets their precedence and associativity.

infixl 6 :+, :-
infixl 7 :*, :/


-- Statements in the Turbo language.
data Stmt
    = String := RealExpr        -- assignment, the string is var name
    | PenDown                   -- set pen to down (touch paper) state
    | PenUp                     -- set pen to up (away from paper) state
    | Turn RealExpr             -- turn counterclockwise by given degrees
                                -- negative angle just means clockwise
    | Forward RealExpr          -- move by given distance units (in current direction)
                                -- negative distance just means backward
                                -- if pen is down, this causes drawing too
                                -- if pen is up, this moves without drawing
    | Seq [Stmt]                -- sequential compound statement. run in given order
    | For String RealExpr RealExpr [Stmt]
      -- for var=expr1 to expr2 do ...
      -- it is up to you whether to evaluate expr2 just once at the beginning
      -- or re-evaluate every iteration
    deriving (Eq, Ord, Read, Show)

infix 5 :=


-- State monad definition and utilities. In particular "modify" is a new
-- addition and should be super convenient.

newtype State s a = StateOf (s -> (s, a))
deState (StateOf stf) = stf
get = StateOf (\s0 -> (s0, s0))
put s = StateOf (\s0 -> (s , ()))
modify f = get >>= \s -> put (f s)
instance Monad (State s) where
    return a = pure a
    StateOf t >>= k = StateOf (\i0 -> case t i0 of
                                  (i1, a) -> deState (k a) i1)
instance Functor (State s) where
    fmap f (StateOf t) = StateOf (\i0 -> case t i0 of (i1, a) -> (i1, f a))
instance Applicative (State s) where
    pure a = StateOf (\i -> (i, a))
    StateOf tf <*> StateOf ta = StateOf
        (\i0 -> case tf i0 of (i1, f) -> case ta i1 of (i2, a) -> (i2, f a))


-- Representation of one command for SVG paths.  In both cases the two fields
-- are for how much to move in the x direction and the y direction.
data SVGPathCmd = MoveTo Double Double -- move without drawing
                | LineTo Double Double -- draw and move
    deriving (Eq, Ord, Read, Show)
