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
    KEsc -> halt g
    KUp  -> continue $ g
      { _screen = Home (bound (menuIndex - 1) 0 (length menuItems - 1 ))
                       menuItems
                       menuItemActions
      }
    KDown -> continue $ g
      { _screen = Home (bound (menuIndex + 1) 0 (length menuItems - 1))
                       menuItems
                       menuItemActions
      }
    KEnter -> continue $ (menuItemActions !! menuIndex) g
    _      -> continue g
  Play -> case key of
    KEnter -> continue $ plantMove g
    KLeft  -> continue $ moveHighlight g Types.GameLeft
    KRight -> continue $ moveHighlight g Types.GameRight
    KUp    -> continue $ moveHighlight g Types.GameUp
    KDown  -> continue $ moveHighlight g Types.GameDown
    _      -> continue g
  (Pause{}) -> continue g
handleEvent g _ = continue g

initGame :: IO Game
initGame = return $ Game { _grid              = initialGrid
                         , _highlightLocation = (1, 0)
                         , _curPlayer         = Player1
                         , _done              = False
                         , _stat              = (0, 0, 0)
                         , _screen            = initialHomeScreen
                         }

initialHomeScreen :: Screen
initialHomeScreen = Home { _curMenuItemIndex = 0
                         , _menuItems        = ["Play", "Quit"]
                         , _menuItemActions      = [id, id]
                         }

startHumanGame :: IO ()
startHumanGame = do
  g          <- initGame
  initialVty <- V.mkVty V.defaultConfig
  void $ customMain initialVty (V.mkVty V.defaultConfig) Nothing app g
