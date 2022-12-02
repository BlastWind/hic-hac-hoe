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
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                )
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import qualified Graphics.Vty                  as V
import           Graphics.Vty
import           Logic
import           TypeConstants
import           Types                          ( FlickerArrow(FlickerArrow)
                                                , Game(..)
                                                , HomeData(..)
                                                , Menu(..)
                                                , PauseData(PauseData, _menu)
                                                , Togglable(toggle)
                                                )
import qualified Types                         as PlayDirection
import           UI


app :: App Game FlickerArrow ()
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return -- do nothing
          , appAttrMap      = const tictactoeAttrMap
          }

handleEvent :: Game -> BrickEvent () FlickerArrow -> EventM () (Next Game)

handleEvent (Home (HomeData menu)) (AppEvent FlickerArrow) =
  continue $ Home $ HomeData $ menu { _arrowStatus = toggle $ _arrowStatus menu
                                    }

handleEvent (Pause pauseData@(PauseData _ menu)) (AppEvent FlickerArrow) =
  continue $ Pause $ pauseData
    { _menu = menu { _arrowStatus = toggle $ _arrowStatus menu }
    }

handleEvent game@(Home (HomeData menu@(Menu curMenuItemIndex menuItems menuItemActions _))) (VtyEvent (V.EvKey key []))
  = case key of
    KUp -> continue $ Home $ HomeData $ menu
      { _curMenuItemIndex = bound (curMenuItemIndex - 1)
                                  0
                                  (length menuItems - 1)
      }
    KDown -> continue $ Home $ HomeData $ menu
      { _curMenuItemIndex = bound (curMenuItemIndex + 1)
                                  0
                                  (length menuItems - 1)
      }
    KEnter -> (menuItemActions !! curMenuItemIndex) game
    _      -> continue game

handleEvent game@(Play playData) (VtyEvent (V.EvKey key [])) = case key of
  KEnter -> continue $ plantMove game
  KLeft  -> continue $ moveHighlight game PlayDirection.Left
  KRight -> continue $ moveHighlight game PlayDirection.Right
  KUp    -> continue $ moveHighlight game PlayDirection.Up
  KDown  -> continue $ moveHighlight game PlayDirection.Down
  KEsc   -> continue $ Pause $ PauseData playData pauseMenu
  _      -> continue game

handleEvent game@(Pause (PauseData playData menu@(Menu curMenuItemIndex menuItems menuItemActions _))) (VtyEvent (V.EvKey key []))
  = case key of
    KUp -> continue $ Pause $ PauseData
      playData
      menu
        { _curMenuItemIndex = bound (curMenuItemIndex - 1)
                                    0
                                    (length menuItems - 1)
        }

    KDown -> continue $ Pause $ PauseData
      playData
      menu
        { _curMenuItemIndex = bound (curMenuItemIndex + 1)
                                    0
                                    (length menuItems - 1)
        }
    KEnter -> (menuItemActions !! curMenuItemIndex) game
    _      -> continue game

handleEvent g _ = continue g

startHumanGame :: IO ()
startHumanGame = do
  initialVty <- V.mkVty V.defaultConfig
  chan       <- newBChan 10
  backgroundForever $ do
    writeBChan chan FlickerArrow
    threadDelay flickerArrowFrequency
  void $ customMain initialVty
                    (V.mkVty V.defaultConfig)
                    (Just chan)
                    app
                    initialHomeGame

backgroundForever :: IO a -> IO ()
backgroundForever a = void $ forkIO . forever $ a
