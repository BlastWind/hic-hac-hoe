module UI
    ( module UI
    ) where
import           Brick                          ( AttrMap
                                                , AttrName
                                                , Widget
                                                , attrMap
                                                , attrName
                                                )
import qualified Brick                         as U
import qualified Brick.Widgets.Border          as B
import           Brick.Widgets.Border           ( borderAttr )
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import qualified Graphics.Vty                  as V
import           Types

highlightBorderAttr, defaultAttr :: AttrName
highlightBorderAttr = attrName "highlightBorderAttr"
defaultAttr = attrName "default"

tictactoeAttrMap :: AttrMap
tictactoeAttrMap = attrMap V.defAttr [(highlightBorderAttr, U.fg V.cyan)]

printTile :: Tile -> String
printTile = maybe " " show

drawGrid :: Game -> Widget ()
drawGrid g =
    withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Tic Tac Toe")
        $ vBox columnWidgets
  where
    columnWidgets =
        [ makeRowWidgets rowTiles rowInd
        | (rowTiles, rowInd) <- zip (_grid g) [(0 :: Int) ..]
        ]
    makeRowWidgets rowTiles rowInd =
        hLimit 27
            $ hBox
            $ [ makeCellWidget tile rowInd colInd
              | (tile, colInd) <- zip rowTiles [(0 :: Int) ..]
              ]
    makeCellWidget tile rowInd colInd =
        (if shouldHighlight
                then overrideAttr borderAttr highlightBorderAttr
                else id
            )
            $ B.border
            $ hCenter
            $ padAll 1
            $ str
            $ printTile tile
        where shouldHighlight = (rowInd, colInd) == _highlightLocation g

drawUI :: Game -> [Widget ()]
drawUI g = [center $ drawGrid g]
