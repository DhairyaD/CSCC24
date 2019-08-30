-- How to use: runghc testQuadtree.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Data.Array
import Data.Word

import PicIO
import qualified Quadtree as S (quadtreeToPic, picToQuadtree)
import QuadtreeDef

-- Re-assert types.
quadtreeToPic :: Quadtree -> Array (Int, Int) Word8
quadtreeToPic = S.quadtreeToPic
picToQuadtree :: Word8 -> Int -> Array (Int, Int) Word8 -> Quadtree
picToQuadtree = S.picToQuadtree

q1tests =
    [ quadtreeToPic quadtree1b
      ~?= listArray ((0,0), (3,3)) [0,0,2,6,0,0,4,2,2,2,11,11,2,2,11,11]
    ]

q2tests =
    [ picToQuadtree 0 1 pic1 ~?= quadtree1a
    , picToQuadtree 2 2 pic1 ~?= quadtree1b
    ]

-- More test cases when marking.

tests = q1tests ++ q2tests

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
