module QuadtreeDef where

import Data.Array
import Data.Word

data Quadtree = QNode Int Int Int Word8 QKids
    deriving (Eq, Show)
-- The fields mean, in order:
--   top left x coordinate
--   top left y coordinate,
--   width (and height, this is a square),
--   "average" colour,
--   child nodes.
-- I.e., QNode x y width avgColour kids

data QKids = Q0 | Q4 Quadtree Quadtree Quadtree Quadtree
    deriving (Eq, Show)

pic1 :: Array (Int, Int) Word8
pic1 = listArray ((0,0), (3,3)) [0,0,2,6,0,0,4,2,1,3,10,11,3,1,11,11]

quadtree1a = QNode 0 0 4 4 (Q4 (QNode 0 0 2 0 Q0) (QNode 0 2 2 4 Q0) (QNode 2 0 2 2 Q0) (QNode 2 2 2 11 Q0))

quadtree1b =
  QNode 0 0 4 4  (Q4
    (QNode 0 0 2 0 Q0)
    (QNode 0 2 2 4 (Q4
      (QNode 0 2 1 2 Q0)
      (QNode 0 3 1 6 Q0)
      (QNode 1 2 1 4 Q0)
      (QNode 1 3 1 2 Q0)))
    (QNode 2 0 2 2 Q0)
    (QNode 2 2 2 11 Q0))
