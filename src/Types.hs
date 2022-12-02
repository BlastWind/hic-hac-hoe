-- Data types and type transformation functions
module Types (module Types) where
    data Player = Player1 | Player2 deriving (Show)
    data TileType = O | X deriving (Show, Eq)
    type Tile = Maybe TileType
    type Grid = [[Tile]]

    type Coordinate = Int
    type Location = (Coordinate, Coordinate)
    type Player1Score = Int 
    type Player2Score = Int
    type MatchesPlayed = Int
    type Stat = (Player1Score, Player2Score, MatchesPlayed)
    
    type ScreenItemIndex = Int
    -- Screen type and corresponding screen data to render screen with
    data Screen = Home ScreenItemIndex | Play | Pause ScreenItemIndex

    data Game = Game { _grid :: Grid, _highlightLocation :: Location, _curPlayer :: Player, _done :: Bool, _stat :: Stat, _screen :: Screen }
    data Direction = Up | Down | Left | Right

    class Togglable a where
        toggle :: a -> a

    instance Togglable Player where
        toggle Player1 = Player2
        toggle Player2 = Player1

    playerToTileType :: Player -> TileType
    playerToTileType Player1 = X
    playerToTileType Player2 = O