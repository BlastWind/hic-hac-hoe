module HumanGame (startHumanGame) where
    import Graphics.Vty
    import qualified Graphics.Vty as V
    import Control.Monad (void)
    import Types
    import UI
    import Logic
    import Brick (App (..), BrickEvent (..), Next, EventM, neverShowCursor, attrMap, halt, continue, attrName, fg, customMain)
    

    app :: App Game () ()
    app = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return -- do nothing
            , appAttrMap = const $ attrMap Graphics.Vty.defAttr [(attrName "ok", fg blue)]
            }

    handleEvent :: Game -> BrickEvent () () -> EventM () (Next Game)
    handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
    handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ moveHighlight g Types.Down
    handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ moveHighlight g Types.Up
    handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveHighlight g Types.Left
    handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ moveHighlight g Types.Right

    handleEvent g _ = continue g

    initGame :: IO Game
    initGame = return $
        Game {
            _grid = [[Just O, Just O, Nothing],
            [Just O, Nothing, Just X],
            [Just O, Just O, Nothing]],
            _highlightLocation = (1, 0),
            _curPlayer = Player1,
            _done = False
        }

    startHumanGame :: IO ()
    startHumanGame = do
        g <- initGame
        initialVty <- V.mkVty V.defaultConfig
        void $ customMain initialVty (V.mkVty V.defaultConfig) Nothing app g