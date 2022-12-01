module UI (module UI) where 
    import Types
    import Brick (Widget)
    import qualified Brick.Widgets.Border.Style as BS
    import qualified Brick.Widgets.Border as B
    import Brick.Widgets.Core
    import Brick.Widgets.Center
    
    
    printTile :: Tile -> String
    printTile = maybe " " show

    drawGrid :: Game -> Widget ()
    drawGrid g = withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Tic Tac Toe")
        $ vBox columnWidgets
        where
            columnWidgets = [makeRowWidget rowTiles rowInd | (rowTiles, rowInd) <- zip (_grid g) [(0 :: Int)..]]
            makeRowWidget row rowInd = hLimit 27 $ hBox $ [B.border $ hCenter $ padAll 1 $ str $ (if (rowInd, colInd) == _highlightLocation g then "highlight" else printTile tile) | (tile, colInd) <- zip row [(0 :: Int)..]]

    drawUI :: Game -> [Widget ()]
    drawUI g = [center $ drawGrid g]
