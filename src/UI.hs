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
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import qualified Graphics.Vty                  as V
import           Types

highlightBorderAttr :: AttrName
highlightBorderAttr = attrName "highlightBorderAttr"

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
        B.border
            $ hCenter
            $ padAll 1
            $ str
            $ (if (rowInd, colInd) == _highlightLocation g
                  then "highlight"
                  else printTile tile
              )

drawUI :: Game -> [Widget ()]
drawUI g = [center $ drawGrid g]
