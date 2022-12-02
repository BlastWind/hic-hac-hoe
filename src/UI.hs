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

drawGrid :: PlayData -> Widget () -- Can't just take in Grid because drawGrid also depends on highlightLocation
drawGrid (PlayData grid highlightLocation _ _) =
    withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Tic Tac Toe")
        $ vBox columnWidgets
  where
    columnWidgets =
        [ makeRowWidgets rowTiles rowInd
        | (rowTiles, rowInd) <- zip grid [(0 :: Int) ..]
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
        where shouldHighlight = (rowInd, colInd) == highlightLocation

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

drawUI :: Game -> [Widget ()]
drawUI (Home (HomeData menu)) =
    [ center $ hLimit 30 $ vLimit 20 $ B.border $ padAll 2 $ vBox
          [ str "Welcome to Hic Hac Hoe"
          , padTop (Pad 1)
          $ hCenter
          $ drawMenu menu
          ]
    ]

drawUI (Play playData@(PlayData _ _ curPlayer stat)) =
    [center $ drawPlayerTurn curPlayer <+> drawGrid playData <+> drawStat stat]

drawUI (Pause (PauseData playData@(PlayData _ _ curPlayer stat) menu))
    = [ center
            $   drawPlayerTurn curPlayer
            <+> (drawGrid playData <=> drawMenu menu
                )
            <+> drawStat stat
      ]

drawMenu :: Menu -> Widget ()
drawMenu (Menu itemInd options _ arrowStatus) = B.borderWithLabel
    (str "Menu")
    (padLeftRight
        3
        (vBox
            (map
                ( padTopBottom 1
                . str
                . (\(ind, s) -> if ind == itemInd
                      then drawArrow arrowStatus ++ s
                      else "   " ++ s -- still want to maintain spacing
                  )
                )
                (enumerate options)
            )
        )
    )



drawArrow :: ArrowStatus -> String
drawArrow On  = "-> "
drawArrow Off = "   "
