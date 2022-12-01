module Logic
    ( module Logic
    ) where
import           Types

directionVector :: Direction -> (Coordinate, Coordinate)
directionVector Types.Up    = (-1, 0)
directionVector Types.Right = (0, 1)
directionVector Types.Down  = (1, 0)
directionVector Types.Left  = (0, -1)

boundLoc :: Location -> Location
boundLoc (x, y) = (boundDigit x, boundDigit y)  where
    boundDigit digit | digit < 0 = 0
                     | digit > 2 = 2
                     | otherwise = digit

addTwoTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTwoTuples (x, y) (u, v) = (x + u, y + v)

moveHighlight :: Game -> Direction -> Game
moveHighlight g dir = g
    { _highlightLocation = boundLoc $ addTwoTuples (directionVector dir)
                                                   (_highlightLocation g)
    }

