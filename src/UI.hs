module UI
    ( module UI
    ) where
import           Brick                          ( AttrMap
                                                , AttrName
                                                , Padding(Pad)
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
import           Logic                          ( enumerate )
import           Types

highlightBorderAttr, defaultAttr :: AttrName
highlightBorderAttr = attrName "highlightBorderAttr"
defaultAttr = attrName "default"

tictactoeAttrMap :: AttrMap
tictactoeAttrMap = attrMap V.defAttr [(highlightBorderAttr, U.fg V.cyan)]

printTile :: Tile -> String
printTile = maybe " " show

-- Can't just take in Grid because drawGrid also depends on highlightLocation
drawGrid :: Game -> Widget ()
drawGrid game =
    withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Tic Tac Toe")
        $ vBox columnWidgets
  where
    columnWidgets =
        [ makeRowWidgets rowTiles rowInd
        | (rowTiles, rowInd) <- zip (_grid game) [(0 :: Int) ..]
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
        where shouldHighlight = (rowInd, colInd) == _highlightLocation game

drawPlayerTurn :: Player -> Widget ()
drawPlayerTurn player =
    B.borderWithLabel (str "Current")
        $ padLeftRight 3
        $ padTopBottom 1
        $ str
        $ show
        $ playerToTileType player

drawStat :: Stat -> Widget ()
drawStat (player1Score, player2Score, matchesPlayed) =
    B.borderWithLabel (str "Stats") $ padAll 1 $ hBox
        [ padRight (Pad 1)
            $ vBox [str "Player1 :", str "Player2 :", str "Played  :"]
        , vBox
            [ str $ show player1Score
            , str $ show player2Score
            , str $ show matchesPlayed
            ]
        ]

drawHome :: Screen -> Widget ()
drawHome (Home 0) = str "jk"

homeMenuOptions :: [String]
homeMenuOptions = ["Play", "Quit"]

    -- vBox [hBox [str "Player 1:", str $ show player1Score], hBox [str "Player 2:", str $ show player2Score], hBox [str "Played:", str $ show matchesPlayed]] 
drawUI :: Game -> [Widget ()]
drawUI game = case _screen game of
    (Home itemInd) ->
        [ center
              $ hLimit 30
              $ vLimit 20
              $ B.border
              $ padAll 2
              $ vBox
              $ [ str "Welcome to Hic Hac Hoe"
                , padTop (Pad 1)
                $ hCenter
                $ B.borderWithLabel (str "Menu")
                $ padLeftRight
                      3
                      (vBox
                          (map
                              ( padTopBottom 1
                              . str
                              . (\(ind, s) -> if ind == itemInd
                                    then "-> " ++ s
                                    else "   " ++ s -- still want to maintain spacing
                                )
                              )
                              (enumerate homeMenuOptions)
                          )
                      )
                ]
        ]
    Play ->
        [ center
              $   drawPlayerTurn (_curPlayer game)
              <+> drawGrid game
              <+> drawStat (_stat game)
        ]
    (Pause _) ->
        [ center
              $   drawPlayerTurn (_curPlayer game)
              <+> drawGrid game
              <+> drawStat (_stat game)
              <+> str "Paused"
        ]


