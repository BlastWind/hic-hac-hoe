-- Data types and type transformation functions
module Types (module Types) where
    import Brick (EventM, Next)
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
    
    type MenuItemIndex = Int

    data Menu = Menu { _curMenuItemIndex :: MenuItemIndex, _menuItems :: [String], _menuItemActions :: [Game -> EventM () (Next Game)] }
    newtype HomeData = HomeData Menu
    data PauseData = PauseData {_lastPlay :: PlayData, _menu :: Menu}
    data PlayData = PlayData {_grid :: Grid, _highlightLocation :: Location, _curPlayer :: Player, _stat :: Stat} 

    data Game = Home HomeData | Play PlayData | Pause PauseData
    data PlayDirection = Up | Down | Left | Right

    class Togglable a where
        toggle :: a -> a

    instance Togglable Player where
        toggle Player1 = Player2
        toggle Player2 = Player1

    playerToTileType :: Player -> TileType
    playerToTileType Player1 = X
    playerToTileType Player2 = O