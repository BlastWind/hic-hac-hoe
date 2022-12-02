module HumanGame
  ( startHumanGame
  , initialGrid
  ) where
import           Brick                          ( App(..)
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , customMain
                                                , halt
                                                , neverShowCursor
                                                )
import           Control.Monad                  ( void )
import qualified Graphics.Vty                  as V
import           Graphics.Vty
import           Logic
import           Types
import           UI


app :: App Game () ()
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return -- do nothing
          , appAttrMap      = const tictactoeAttrMap
          }

handleEvent :: Game -> BrickEvent () () -> EventM () (Next Game)
handleEvent g (VtyEvent (V.EvKey key [])) = case _screen g of
  (Home menuIndex menuItems menuItemActions) -> case key of
    KUp -> continue $ g
      { _screen = Home (bound (menuIndex - 1) 0 (length menuItems - 1))
                       menuItems
                       menuItemActions
      }
    KDown -> continue $ g
      { _screen = Home (bound (menuIndex + 1) 0 (length menuItems - 1))
                       menuItems
                       menuItemActions
      }
    KEnter -> (menuItemActions !! menuIndex) g
    _      -> continue g
  Play -> case key of
    KEnter -> continue $ plantMove g
    KLeft  -> continue $ moveHighlight g Types.GameLeft
    KRight -> continue $ moveHighlight g Types.GameRight
    KUp    -> continue $ moveHighlight g Types.GameUp
    KDown  -> continue $ moveHighlight g Types.GameDown
    KEsc   -> continue g { _screen = initialPauseScreen }
    _      -> continue g
  (Pause menuIndex menuItems menuItemActions) -> case key of
    KUp -> continue $ g
      { _screen = Pause (bound (menuIndex - 1) 0 (length menuItems - 1))
                       menuItems
                       menuItemActions
      }
    KDown -> continue $ g
      { _screen = Pause (bound (menuIndex + 1) 0 (length menuItems - 1))
                       menuItems
                       menuItemActions
      }
    KEnter -> (menuItemActions !! menuIndex) g
    _      -> continue g
handleEvent g _ = continue g

initialHomeScreen :: Screen
initialHomeScreen = Home
  { _curMenuItemIndex = 0
  , _menuItems        = ["Play", "Quit"]
  , _menuItemActions  = [const $ continue initialPlayScreenGame, halt]
  }

initialHomeGame :: Game
initialHomeGame = Game { _grid              = initialGrid
                       , _highlightLocation = (1, 0)
                       , _curPlayer         = Player1
                       , _done              = False
                       , _stat              = (0, 0, 0)
                       , _screen            = initialHomeScreen
                       }

initialPlayScreenGame :: Game
initialPlayScreenGame = Game { _grid              = initialGrid
                             , _highlightLocation = (1, 0)
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

startHumanGame :: IO ()
startHumanGame = do
  initialVty <- V.mkVty V.defaultConfig
  void $ customMain initialVty
                    (V.mkVty V.defaultConfig)
                    Nothing
                    app
                    initialHomeGame
