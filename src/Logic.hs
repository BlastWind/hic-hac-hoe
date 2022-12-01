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

playerToTileType :: Player -> TileType
playerToTileType Player1 = X
playerToTileType Player2 = O

class Togglable a where 
    toggle :: a -> a

instance Togglable Player where 
    toggle Player1 = Player2
    toggle Player2 = Player1
    

plantMove :: Game -> Game
plantMove game = game
    { _curPlayer = toggle $ _curPlayer game
    , _done      = False
    , _grid      = updateGrid (_grid game) (_highlightLocation game) (Just $ playerToTileType (_curPlayer game))
    }

updateGrid :: Grid -> (Coordinate, Coordinate) -> Tile -> Grid
updateGrid grid (row, col) tile = [if rowInd == row then setAt rowTiles col tile  else rowTiles | (rowTiles, rowInd) <- zip grid [0..]]

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs