module HumanGame
    ( startHumanGame, initialGrid
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
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) =
    continue $ moveHighlight g Types.Down
handleEvent g (VtyEvent (V.EvKey V.KUp [])) =
    continue $ moveHighlight g Types.Up
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) =
    continue $ moveHighlight g Types.Left
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
    continue $ moveHighlight g Types.Right
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) =
    continue $ plantMove g
handleEvent g _ = continue g

initGame :: IO Game
initGame = return $ Game
    { _grid              = initialGrid
    , _highlightLocation = (1, 0)
    , _curPlayer         = Player1
    , _done              = False
    , _stat              = (0, 0, 0)
    }

startHumanGame :: IO ()
startHumanGame = do
    g          <- initGame
    initialVty <- V.mkVty V.defaultConfig
    void $ customMain initialVty (V.mkVty V.defaultConfig) Nothing app g
