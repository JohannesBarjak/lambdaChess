{-# LANGUAGE TemplateHaskell #-}
module Main where

import LambdaChess.Board
import Brick

import Brick.Widgets.Center
import Brick.Widgets.Table

import Control.Comonad.Store
import Control.Lens

import Control.Monad (void)
import Data.Char (toLower)
import Graphics.Vty

data ChessGame = ChessGame
  { _cursor :: Square
  , _board :: Chessboard
  }

makeLenses ''ChessGame

newGame :: ChessGame
newGame = ChessGame
  { _cursor = Square A A
  , _board = standardChessboard
  }

initialApp :: App ChessGame e ()
initialApp = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = chessAttrMap
  }

main :: IO ()
main = void $ defaultMain initialApp newGame

draw :: ChessGame -> [Widget n]
draw gameState =
  [ vCenter . hCenter
  $ renderWidgetBoard
  $ highlightSelected (gameState^.cursor)
  $ colorCells
  $ widgetBoard (gameState^.board)
  ]

renderWidgetBoard :: Board (Widget n) -> Widget n
renderWidgetBoard
  = renderTable
  . columnBorders False . rowBorders False
  . table . reverse . toGrid

handleEvent :: BrickEvent n e -> EventM n ChessGame ()
handleEvent e = case e of
  (VtyEvent (EvKey key _)) -> case key of
    KChar c | toLower c == 'q' -> halt

    KUp -> modify $ cursor %~ sqUp
    KDown -> modify $ cursor %~ sqDown
    KLeft -> modify $ cursor %~ sqLeft
    KRight -> modify $ cursor %~ sqRight

    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

highlightSelected :: Square -> Board (Widget n) -> Board (Widget n)
highlightSelected cur bd = seek cur bd&bdSel %~ withAttr selectedAttr

colorCells :: Board (Widget n) -> Board (Widget n)
colorCells = extend colorSelected

  where colorSelected bd
          | even squareSum = modifyDefAttr (`withBackColor` rgbColor @Integer 135 46 46) (extract bd)
          | otherwise = modifyDefAttr (`withBackColor` rgbColor @Integer 234 187 162) (extract bd)

          where squareSum = fromEnum (bd^.square.file) + fromEnum (bd^.square.rank)

widgetBoard :: Chessboard -> Board (Widget n)
widgetBoard = fmap
  $   maybe id (colorWidget . col)
  <*> padLeftRight 2 . padAll 1 . maybe emptyCell (toWidget . piece)

  where toWidget Pawn   = pawnWidget
        toWidget Queen  = queenWidget
        toWidget King   = kingWidget
        toWidget Rook   = rookWidget
        toWidget Bishop = bishopWidget
        toWidget Knight = knightWidget

        colorWidget White = modifyDefAttr (flip withStyle bold . (`withForeColor` rgbColor @Integer 235 219 178))
        colorWidget Black = modifyDefAttr (flip withStyle bold . (`withForeColor` rgbColor @Integer 29 32 33))

chessAttrMap :: ChessGame -> AttrMap
chessAttrMap = const $ attrMap defAttr
  [ (selectedAttr, bg $ rgbColor @Integer 146 131 116)
  ]

selectedAttr :: AttrName
selectedAttr = attrName "selected"

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
