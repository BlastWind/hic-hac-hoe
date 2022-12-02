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
import           Types                          ( Game(..)
                                                , HomeData(..)
                                                , Menu(..)
                                                , PauseData(PauseData)
                                                )
import qualified Types                         as PlayDirection
import           UI


app :: App Game () ()
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return -- do nothing
          , appAttrMap      = const tictactoeAttrMap
          }

handleEvent :: Game -> BrickEvent () () -> EventM () (Next Game)
handleEvent game@(Home (HomeData (Menu menuIndex menuItems menuItemActions))) (VtyEvent (V.EvKey key []))
  = case key of
    KUp -> continue $ Home $ HomeData $ Menu (bound (menuIndex - 1) 0 (length menuItems - 1)) menuItems menuItemActions
    KDown -> continue $ Home $ HomeData $ Menu (bound (menuIndex + 1) 0 (length menuItems - 1)) menuItems menuItemActions
    KEnter -> (menuItemActions !! menuIndex) game
    _      -> continue game

handleEvent game@(Play playData) (VtyEvent (V.EvKey key [])) = case key of
  KEnter -> continue $ plantMove game
  KLeft  -> continue $ moveHighlight game PlayDirection.Left
  KRight -> continue $ moveHighlight game PlayDirection.Right
  KUp    -> continue $ moveHighlight game PlayDirection.Up
  KDown  -> continue $ moveHighlight game PlayDirection.Down
  KEsc   -> continue $ Pause $ PauseData playData pauseMenu
  _      -> continue game

handleEvent game@(Pause (PauseData playData (Menu menuIndex menuItems menuItemActions))) (VtyEvent (V.EvKey key []))
  = case key of
    KUp -> continue $ Pause $ PauseData
      playData
      (Menu (bound (menuIndex - 1) 0 (length menuItems - 1)) menuItems menuItemActions)
    KDown -> continue $ Pause $ PauseData
      playData
      (Menu (bound (menuIndex + 1) 0 (length menuItems - 1)) menuItems menuItemActions)
    KEnter -> (menuItemActions !! menuIndex) game
    _      -> continue game

handleEvent g _ = continue g

startHumanGame :: IO ()
startHumanGame = do
  initialVty <- V.mkVty V.defaultConfig
  void $ customMain initialVty
                    (V.mkVty V.defaultConfig)
                    Nothing
                    app
                    initialHomeGame
