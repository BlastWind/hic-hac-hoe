module Logic
    ( module Logic
    ) where
import           Data.List                      ( transpose )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           TypeConstants
import           Types

directionVector :: PlayDirection -> (Coordinate, Coordinate)
directionVector Types.Up    = (-1, 0)
directionVector Types.Right = (0, 1)
directionVector Types.Down  = (1, 0)
directionVector Types.Left  = (0, -1)

boundLoc :: Location -> Location
boundLoc (x, y) = (bound x 0 2, bound y 0 2)

bound :: Ord a => a -> a -> a -> a
bound x lowerBound upperBound | x < lowerBound = lowerBound
                              | x > upperBound = upperBound
                              | otherwise      = x

addTwoTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTwoTuples (x, y) (u, v) = (x + u, y + v)

moveHighlight :: Game -> PlayDirection -> Game
moveHighlight (Play (PlayData grid prevHighlightLocation player stat)) dir =
    Play PlayData
        { _grid              = grid
        , _curPlayer            = player
        , _stat              = stat
        , _highlightLocation = boundLoc $ addTwoTuples (directionVector dir)
                                                       prevHighlightLocation
        }
moveHighlight g _ = g


plantMove :: Game -> Game
plantMove (Play (PlayData prevGrid highlightLocation prevPlayer prevStat)) =
    Play PlayData { _curPlayer         = finalPlayer
                  , _grid              = finalGrid
                  , _stat              = finalStat
                  , _highlightLocation = highlightLocation
                  }
  where
    isMoveValid          = isNothing $ (prevGrid !! plantAtX) !! plantAtY
    (plantAtX, plantAtY) = highlightLocation
    gridAfterMove        = if isMoveValid
        then updateGrid prevGrid
                        highlightLocation
                        (Just $ playerToTileType prevPlayer)
        else prevGrid
    didCurPlayerWin = didPlayerWin gridAfterMove prevPlayer
    isGameStuck     = not didCurPlayerWin && isGridFilled gridAfterMove
    finalGrid | didCurPlayerWin = initialGrid
              | isGameStuck     = initialGrid
              | otherwise       = gridAfterMove -- proceed as usual
    finalPlayer | not isMoveValid = prevPlayer
                |
                -- give start to winner
                  didCurPlayerWin = prevPlayer
                | isGameStuck     = toggle $ prevPlayer
                | otherwise       = toggle $ prevPlayer
    finalStat
        | didCurPlayerWin = case prevPlayer of
            Player1 -> (player1Score + 1, player2Score, matchesPlayed + 1)
            Player2 -> (player1Score, player2Score + 1, matchesPlayed + 1)
        | isGameStuck = (player1Score, player2Score, matchesPlayed + 1)
        | otherwise = prevStat
        where (player1Score, player2Score, matchesPlayed) = prevStat
plantMove g = g

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
