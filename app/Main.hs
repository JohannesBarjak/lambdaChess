module Main where

import Board
import Brick

import Brick.Widgets.Center
import Brick.Widgets.Table

import Control.Monad (void)
import Graphics.Vty (defAttr)

data ChessGame = ChessGame { board :: Chessboard }

newGame :: ChessGame
newGame = ChessGame { board = standardChessboard }

ui :: String -> Widget ()
ui = str

main :: IO ()
main = void $ defaultMain initialApp newGame

initialApp :: App ChessGame e ()
initialApp = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const $ attrMap defAttr []
  }

draw :: ChessGame -> [Widget n]
draw (ChessGame g) = [hCenter $ renderWidgetBoard $ padLeftRight 1 <$> widgetBoard g]

renderWidgetBoard :: Board (Widget n) -> Widget n
renderWidgetBoard = renderTable . table . toGrid

handleEvent :: BrickEvent n e -> EventM n ChessGame ()
handleEvent _ = continueWithoutRedraw

widgetBoard :: Chessboard -> Board (Widget n)
widgetBoard = fmap (maybe emptyCell toWidget)
  where toWidget (Pawn   _) = pawnWidget
        toWidget (Queen  _) = queenWidget
        toWidget (King   _) = kingWidget
        toWidget (Rook   _) = rookWidget
        toWidget (Bishop _) = bishopWidget
        toWidget (Knight _) = knightWidget

queenWidget :: Widget n
queenWidget = vBox $ map str
  [ ")^("
  , "|_|"
  ]

kingWidget :: Widget n
kingWidget = vBox $ map str
  [ ")+("
  , "|_|"
  ]

knightWidget :: Widget n
knightWidget = vBox $ map str
  [ "/^)"
  , "(_|"
  ]

pawnWidget :: Widget n
pawnWidget = vBox $ map str
  [ " O "
  , "(_)"
  ]

rookWidget :: Widget n
rookWidget = vBox $ map str
  [ "|\"|"
  , "|_|"
  ]

bishopWidget :: Widget n
bishopWidget = vBox $ map str
  [ "(\\)"
  , "|_|"
  ]

emptyCell :: Widget n
emptyCell = vBox $ replicate 2 (str "   ")
