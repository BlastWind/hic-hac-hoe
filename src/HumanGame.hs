module HumanGame
  ( startHumanGame
  ) where
import           Brick                          ( App(..)
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , customMain
                                                , neverShowCursor
                                                )
import           Control.Monad                  ( void )
import qualified Graphics.Vty                  as V
import           Graphics.Vty
import           Logic
import           TypeConstants
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
handleEvent g (VtyEvent (V.EvKey key [])) = case _screen g of -- listen to different events and do different things with respect to Screen type
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

startHumanGame :: IO ()
startHumanGame = do
  initialVty <- V.mkVty V.defaultConfig
  void $ customMain initialVty
                    (V.mkVty V.defaultConfig)
                    Nothing
                    app
                    initialHomeGame
