module PlayTurbo where

import Numeric (showFFloat)

import TurboDef
import Turbo

-- Run a Turbo program (using your runTurbo), save the SVG path commands in an
-- SVG file.  You can load the SVG file in a web browser to see the drawing.
-- The drawing space shown is 400x300, origin at centre.
svg :: FilePath -> Stmt -> IO ()
svg filename stmt = writeFile filename (prologue ++ path epilogue)
  where
    prologue =
        "<?xml version='1.0' encoding='UTF-8'?>\n\
        \<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n\
        \'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\'>\n\
        \<svg version='1.1' xmlns='http://www.w3.org/2000/svg'\n\
        \width='400px' height='300px'>\n\
        \<g transform='translate(200, 150) scale(1, -1)'>\n"
    epilogue = "</g></svg>\n"
    cmds = runTurbo stmt
    path = ("<path fill='none' stroke='black' stroke-width='1' d='M 0 0 " ++)
           . foldr (.) ("'/>\n" ++) (map cmdToSVG cmds)
    cmdToSVG (MoveTo x y) = ("m " ++) . showsCoord x y
    cmdToSVG (LineTo x y) = ("l " ++) . showsCoord x y
    showsCoord x y = showFFloat (Just 0) x . (" " ++)
                     . showFFloat (Just 0) y . (" " ++)

square = Seq [ Forward (RLit 50)
             , Turn (RLit 90)
             , Forward (RLit 50)
             , PenDown
             , For "i" (RLit 0) (RLit 3)
               [ Turn (RLit 90)
               , Forward (RLit 100)
               ]
             ]

pentagon = Seq [ Forward (RLit 50)
               , Turn (RLit 126)
               , PenDown
               , For "i" (RLit 0) (RLit 4)
                 [ Forward (RLit 58.78)
                 , Turn (RLit 72)
                 ]
               ]

spiral = Seq [ PenDown
             , "s" := RLit 12
             , For "i" (RLit 0) (RLit 60)
               [ Forward (RVar "s")
               , "s" := RVar "s" :* RLit 0.99
               , Turn (RVar "i" :* RLit 0.8)
               ]
             ]
