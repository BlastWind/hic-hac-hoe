module Types (module Types) where
    data Player = Player1 | Player2 deriving (Show)
    data TileType = O | X
    type Tile = Maybe TileType
    type Grid = [[Tile]]

    type Coordinate = Int
    type Location = (Coordinate, Coordinate)
    data Game = Game { _grid :: Grid, _highlightLocation :: Location, _curPlayer :: Player, _done :: Bool }
    data Direction = Up | Down | Left | Right

    instance Show TileType where
      show O = "O"
      show X = "X"