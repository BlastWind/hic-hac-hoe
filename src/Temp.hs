module Temp where

    type Row = Integer
    type Col = Integer
    type Coordinate = (Row, Col) -- 0-indexed
    type BoardDims = Coordinate -- 1-indexed

    data Cell = O | X | Empty deriving (Eq, Show)
    data Move = Move Coordinate Cell deriving (Eq, Show)
    data Board = Board BoardDims [Cell] deriving (Eq, Show)

    to1D :: Coordinate -> BoardDims -> Integer
    to1D (x, y) (_, c) = (x * c) + y

    to2D :: Integer -> BoardDims -> Coordinate
    to2D i (_, c) = divMod i c

    isCoordInBoard :: Coordinate -> BoardDims -> Bool
    isCoordInBoard (x, y) (r, c) = x >= 0 && y >= 0 && x < r && y < c

    boardAt :: Board -> Coordinate -> Cell
    boardAt (Board bDims cells) c = cells !! fromInteger (to1D c bDims)

    dirs :: [(Integer, Integer)]
    dirs = [(1,0), (-1, 0), (0, 1), (0, -1)] -- set A <*> set B is A X B

    -- Winning condition: 3 consecutive connections of the same character
    didMoveWin :: Board -> Move -> Bool
    didMoveWin b@(Board bDims _ ) (Move (startX, startY) moveType) = any ((>=3) . collectDists (startX, startY) (0::Integer)) dirs
        where
              -- we also have to count the cells toward (-dirX, -dirY)
              collectDists base dist (dirX, dirY) = collectDist base dist (dirX, dirY) + collectDist base dist (-dirX, -dirY)
              -- counts the number of consecutive moves with type == moveType made in a direction
              collectDist (baseX, baseY) dist dir@(dirX, dirY)
                | dist >= 3 = 3
                | not $ isCoordInBoard (baseX, baseY) bDims = dist
                | boardAt b (baseX, baseY) /= moveType = dist
                | otherwise =  collectDist (baseX + dirX, baseY + dirY) (dist + 1) dir


    setAt :: Integer -> a -> [a] -> [a]
    setAt 0 a xs = a : xs
    setAt travel a (x:xs) = x : setAt (travel-1) a xs
    setAt _ _ [] = []

    moveBoard :: Board -> Move -> Board
    moveBoard (Board boardDims cells) (Move coord cellType) = Board boardDims $ setAt (to1D coord boardDims) cellType cells

    -- row -> col -> Board [Cell] of CellType Empty
    defaultBoard :: BoardDims -> Board
    defaultBoard (r, c) = Board (r, c) $ replicate (fromInteger (r * c)) Empty
    
    testBoard = Board (3, 3) [O, Empty, Empty, O, Empty, Empty,O, Empty, Empty ]
