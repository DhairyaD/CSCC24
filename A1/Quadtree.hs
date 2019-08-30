module Quadtree where

import Data.Array
import Data.Word

import QuadtreeDef

-- helper
createQuadtree :: Quadtree -> [((Int, Int), Word8)]
createQuadtree (QNode (-1) (-1) _ _ _) = [((0,0),0)]
createQuadtree (QNode x y width grayscaleVal Q0) =  [((xcoord, ycoord), grayscaleVal) | a <- [0..width-1], b <- [0..width-1], let xcoord = x + a, let ycoord = y + b, xcoord >= 0, ycoord >= 0]
createQuadtree (QNode x y width grayscaleVal (Q4 firstchild secondchild thirdchild fourthchild)) = createQuadtree firstchild ++ createQuadtree secondchild ++ createQuadtree thirdchild ++ createQuadtree fourthchild

quadtreeToPic :: Quadtree -> Array (Int, Int) Word8
quadtreeToPic (QNode x y width grayscaleVal childNodes) = array ((x,y), (x+width-1, y+width-1)) (createQuadtree (QNode x y width grayscaleVal childNodes))


-- solution version: 
picToQuadtree :: Word8                    -- threshold
              -> Int                      -- depth cap
              -> Array (Int, Int) Word8   -- image
              -> Quadtree
picToQuadtree threshold maxDepth pic
    | maxDepth == 0 || diff pic <= threshold = QNode x y width colour Q0
    | otherwise = QNode x y width colour (Q4 q00 q01 q10 q11)
  where
    ((x,y), (x1,_)) = bounds pic
    width = x1 - x + 1
    halfWidth = width `div` 2
    colour = round (average (elems pic))
    recurse x y = picToQuadtree threshold (maxDepth - 1) (slice pic x y halfWidth)
    q00 = recurse x y
    q01 = recurse x (y+halfWidth)
    q10 = recurse (x+halfWidth) y
    q11 = recurse (x+halfWidth) (y+halfWidth)

diff pic = maximum pixels - minimum pixels
  where
    pixels = elems pic

slice pic x y width = ixmap ((x,y), (x1, y1)) id pic
  where
    x1 = x+width-1
    y1 = y+width-1

average xs = sum (map realToFrac xs) / realToFrac (length xs)

