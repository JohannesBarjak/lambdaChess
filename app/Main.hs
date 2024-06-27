{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Board
import Brick

import Brick.Widgets.Center
import Brick.Widgets.Table

import Control.Comonad.Store
import Control.Lens

import Control.Monad (void)
import Data.Char (toLower)
import Graphics.Vty

data ChessGame = ChessGame
  { cursor :: Square
  , board :: Chessboard
  }

newGame :: ChessGame
newGame = ChessGame
  { cursor = Square A A
  , board = standardChessboard
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
  $ highlightSelected gameState.cursor
  $ widgetBoard gameState.board
  ]

renderWidgetBoard :: Board (Widget n) -> Widget n
renderWidgetBoard = renderTable . table . reverse . toGrid

handleEvent :: BrickEvent n e -> EventM n ChessGame ()
handleEvent e = case e of
  (VtyEvent (EvKey key _)) -> case key of
    KChar c | toLower c == 'q' -> halt
    KUp -> do modify \gs -> gs{cursor = sqUp gs.cursor}
    KDown -> modify \gs -> gs{cursor = sqDown gs.cursor}
    KLeft -> modify \gs -> gs{cursor = sqLeft gs.cursor}
    KRight -> modify \gs -> gs{cursor = sqRight gs.cursor}
    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

highlightSelected :: Square -> Board (Widget n) -> Board (Widget n)
highlightSelected cur board = seek cur board&bdSel %~ forceAttr selectedAttr

widgetBoard :: Chessboard -> Board (Widget n)
widgetBoard = fmap
  $   maybe id (colorAttr . col)
  <*> padLeftRight 1 . maybe emptyCell (toWidget . piece)

  where toWidget Pawn   = pawnWidget
        toWidget Queen  = queenWidget
        toWidget King   = kingWidget
        toWidget Rook   = rookWidget
        toWidget Bishop = bishopWidget
        toWidget Knight = knightWidget

        colorAttr Black = withAttr blackPieceAttr
        colorAttr White = withAttr whitePieceAttr

chessAttrMap :: ChessGame -> AttrMap
chessAttrMap = const $ attrMap defAttr
  [ (selectedAttr, bg $ rgbColor @Integer 140 140 140)
  , (whitePieceAttr, fg white)
  , (blackPieceAttr, fg black)
  , (blackTileAttr, bg black)
  , (whiteTileAttr, bg white)
  ]

selectedAttr :: AttrName
selectedAttr = attrName "selected"

whitePieceAttr :: AttrName
whitePieceAttr = attrName "whitePiece"

blackPieceAttr :: AttrName
blackPieceAttr = attrName "blackPiece"

whiteTileAttr :: AttrName
whiteTileAttr = attrName "whiteTile"

blackTileAttr :: AttrName
blackTileAttr = attrName "blackAttr"

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
