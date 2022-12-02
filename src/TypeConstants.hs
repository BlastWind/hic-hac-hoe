-- For constant screens, games, grids, etc.
module TypeConstants
    ( module TypeConstants
    ) where
import           Brick                          ( continue
                                                , halt
                                                )
import           Types
initialGrid :: Grid
-- [ [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing] ]
initialGrid = replicate 3 (replicate 3 Nothing)

initialHomeScreen :: Screen
initialHomeScreen = Home
    { _curMenuItemIndex = 0
    , _menuItems        = ["Play", "Quit"]
    , _menuItemActions  = [const $ continue initialPlayScreenGame, halt]
    }

initialHomeGame :: Game
initialHomeGame = Game { _grid              = initialGrid
                       , _highlightLocation = (0, 0)
                       , _curPlayer         = Player1
                       , _done              = False
                       , _stat              = (0, 0, 0)
                       , _screen            = initialHomeScreen
                       }

initialPlayScreenGame :: Game
initialPlayScreenGame = Game { _grid              = initialGrid
                             , _highlightLocation = (0, 0)
                             , _curPlayer         = Player1
                             , _done              = False
                             , _stat              = (0, 0, 0)
                             , _screen            = Play
                             }

initialPauseScreen :: Screen
initialPauseScreen = Pause
    { _curMenuItemIndex = 0
    , _menuItems        = ["Resume", "Return to Home"]
    , _menuItemActions  = [ \g -> continue g { _screen = Play }
                          , const $ continue initialHomeGame
                          ]
    }
