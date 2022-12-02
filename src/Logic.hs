module Logic
    ( module Logic
    ) where
import           Data.List                      ( transpose )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           TypeConstants
import           Types

directionVector :: GameDirection -> (Coordinate, Coordinate)
directionVector Types.GameUp    = (-1, 0)
directionVector Types.GameRight = (0, 1)
directionVector Types.GameDown  = (1, 0)
directionVector Types.GameLeft  = (0, -1)

boundLoc :: Location -> Location
boundLoc (x, y) = (bound x 0 2, bound y 0 2)

bound :: Ord a => a -> a -> a -> a
bound x lowerBound upperBound | x < lowerBound = lowerBound
                              | x > upperBound = upperBound
                              | otherwise      = x

addTwoTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTwoTuples (x, y) (u, v) = (x + u, y + v)

moveHighlight :: Game -> GameDirection -> Game
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

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]
