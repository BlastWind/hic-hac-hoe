module Logic
    ( module Logic
    ) where
import           Data.List                      ( transpose )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Types

initialGrid :: Grid
-- [ [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing] ]
initialGrid = replicate 3 (replicate 3 Nothing)

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

plantMove :: Game -> Game
plantMove game = game { _curPlayer = finalPlayer
                      , _done      = False
                      , _grid      = finalGrid
                      , _stat      = finalStat
                      }
  where
    isMoveValid          = isNothing $ (_grid game !! plantAtX) !! plantAtY
    (plantAtX, plantAtY) = _highlightLocation game
    gridAfterMove        = if isMoveValid
        then updateGrid (_grid game)
                        (_highlightLocation game)
                        (Just $ playerToTileType (_curPlayer game))
        else _grid game
    didCurPlayerWin = didPlayerWin gridAfterMove (_curPlayer game)
    isGameStuck     = not didCurPlayerWin && isGridFilled gridAfterMove
    finalGrid | didCurPlayerWin = initialGrid
              | isGameStuck     = initialGrid
              | otherwise       = gridAfterMove -- proceed as usual
    finalPlayer | not isMoveValid = _curPlayer game
                |
                -- give start to winner
                  didCurPlayerWin = _curPlayer game
                | isGameStuck     = toggle $ _curPlayer game
                | otherwise       = toggle $ _curPlayer game
    finalStat
        | didCurPlayerWin = case _curPlayer game of
            Player1 -> (player1Score + 1, player2Score, matchesPlayed + 1)
            Player2 -> (player1Score, player2Score + 1, matchesPlayed + 1)
        | isGameStuck = (player1Score, player2Score, matchesPlayed + 1)
        | otherwise = _stat game
        where (player1Score, player2Score, matchesPlayed) = _stat game


isGridFilled :: Grid -> Bool
isGridFilled = all (all isJust)

didPlayerWin :: Grid -> Player -> Bool
didPlayerWin grid player =
    any (all isTileWin) rows
        || any (all isTileWin) columns
        || all isTileWin upperLeftTobottomRightDiagonal
        || all isTileWin bottomLeftToTopRightDiagonal
  where
    columns = transpose rows
    rows    = grid
    upperLeftTobottomRightDiagonal =
        [head (head grid), grid !! 1 !! 1, grid !! 2 !! 2]
    bottomLeftToTopRightDiagonal =
        [head (grid !! 2), grid !! 1 !! 1, head grid !! 2]
    isTileWin = (Just (playerToTileType player) ==)


updateGrid :: Grid -> (Coordinate, Coordinate) -> Tile -> Grid
updateGrid grid (row, col) tile =
    [ if rowInd == row then setAt rowTiles col tile else rowTiles
    | (rowTiles, rowInd) <- zip grid [0 ..]
    ]

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs
