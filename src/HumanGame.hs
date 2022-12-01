module HumanGame where
    import Brick
    import Data.Bool (bool)
    import Graphics.Vty
    import qualified Graphics.Vty as V
    import Control.Monad (void)
    import qualified Brick.Widgets.Border.Style as BS
    import qualified Brick.Widgets.Border as B
    import qualified Brick.Widgets.Center as C
    import Brick.Widgets.Center
    
    data Player = Player1 | Player2
    data TileType = O | X
    type Tile = Maybe TileType
    type Grid = [[Tile]]

    data Game = Game { _grid :: Grid, _curPlayer :: Player, _done :: Bool }

    app :: App Game () ()
    app = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return -- do nothing
            , appAttrMap = const $ attrMap Graphics.Vty.defAttr [(attrName "ok", fg blue)]
            }

    handleEvent :: Game -> BrickEvent () () -> EventM () (Next Game)
    handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
    handleEvent g _ = continue g

    drawUI :: Game -> [Widget ()]
    drawUI g = [center $ drawGrid g]

    instance Show TileType where
      show O = "O"
      show X = "X"

    printTile :: Tile -> String
    printTile = maybe " " show

    drawGrid :: Game -> Widget ()
    drawGrid g = withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Tic Tac Toe")
        $ vBox columnWidgets
        where
            columnWidgets = [makeRowWidget r | r <- _grid g]
            makeRowWidget row = hLimit 27 $ hBox $ [B.border $ hCenter $ padAll 1 $ str $ printTile tile | tile <- row]


    initGame :: IO Game
    initGame = return $
        Game {
            _grid = [[Just O, Just O, Nothing],
            [Just O, Nothing, Just X],
            [Just O, Just O, Nothing]],
            _curPlayer = Player1,
            _done = False
        }

    startHumanGame :: IO ()
    startHumanGame = do
        g <- initGame
        initialVty <- V.mkVty V.defaultConfig
        void $ customMain initialVty (V.mkVty V.defaultConfig) Nothing app g