-- For constant screens, games, grids, etc.
module TypeConstants
    ( module TypeConstants
    ) where
import           Brick                          ( EventM
                                                , Next
                                                , continue
                                                , halt
                                                )
import           Types
initialGrid :: Grid
-- [ [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing]
--   [Nothing, Nothing, Nothing] ]
initialGrid = replicate 3 (replicate 3 Nothing)


initialHomeGame :: Game
initialHomeGame = Home
    (HomeData Menu
        { _curMenuItemIndex = 0
        , _menuItems        = ["Play", "Quit"]
        , _menuItemActions  = [const $ continue initialPlayScreenGame, halt]
        , _arrowStatus      = Off
        }
    )

initialPlayScreenGame :: Game
initialPlayScreenGame = Play
    (PlayData { _grid              = initialGrid
              , _highlightLocation = (0, 0)
              , _curPlayer         = Player1
              , _stat              = (0, 0, 0)
              }
    )

pauseMenu :: Menu
pauseMenu = Menu
    { _curMenuItemIndex = 0
    , _menuItems        = ["Resume", "Return to Home"]
    , _menuItemActions  = [resumePlay, const $ continue initialHomeGame]
    , _arrowStatus      = Off
    }

resumePlay :: Game -> EventM () (Next Game)
resumePlay (Pause (PauseData lastPlay _)) = continue $ Play lastPlay
resumePlay g                              = continue g

flickerArrowFrequency :: Int
flickerArrowFrequency = 1000 * 400 -- 400 milliseconds