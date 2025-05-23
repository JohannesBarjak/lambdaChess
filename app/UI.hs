{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
module UI
  ( ChessGame
  , newGame
  , initialApp
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Table

import Control.Comonad.Store
import Control.Lens
import Control.Monad
import Data.Foldable (traverse_)

import Data.Bool (bool)
import Data.Char (toLower)
import Graphics.Vty

import LambdaChess
import LambdaChess.Board

data ChessGame = ChessGame
  { _cursor :: Square
  , _board :: Chessboard
  , _selected :: Bool
  , _turn :: Player
  }

makeClassy ''ChessGame

newGame :: ChessGame
newGame = ChessGame
  { _cursor = Square A A
  , _board = standardChessboard
  , _selected = False
  , _turn = White
  }

initialApp :: App ChessGame e ()
initialApp = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = chessAttrMap
  }

-- Main game pipeline.
draw :: ChessGame -> [Widget n]
draw gState =
  [ renderWidgetBoard
  . bool id (highlightSelected (moves $ gState^.board)) (gState^.selected)
  . highlightCursor (gState^.cursor)
  . colorCells
  . widgetBoard $ gState^.board
  ]

-- Final board image to draw.
renderWidgetBoard :: Board (Widget n) -> Widget n
renderWidgetBoard
  = vCenter . hCenter
  . renderTable
  . columnBorders False . rowBorders False
  . table . reverse . toGrid

handleEvent :: BrickEvent n e -> EventM n ChessGame ()
handleEvent = \case
  (VtyEvent (EvKey key _)) -> case key of
    KChar c | toLower c == 'q' -> halt

    KUp    -> traverse_ (assign cursor) . sqUp =<< use cursor
    KDown  -> traverse_ (assign cursor) . sqDown =<< use cursor
    KLeft  -> traverse_ (assign cursor) . sqLeft =<< use cursor
    KRight -> traverse_ (assign cursor) . sqRight =<< use cursor

    KEnter -> do
      sel <- use selected
      cur <- use cursor

      bd <- use board
      tn <- use turn

      if sel then do
        when (cur `elem` moves bd) do
            board %= move cur
            selected .= False
            turn %= otherPlayer

        when (cur == pos bd) do
          selected .= False

      else when (Just tn == (col <$> peek cur bd)) $
          unless (null $ moves (seek cur bd)) do
            board %= seek cur
            selected .= True

    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

-- Invert chess color.
otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

highlightCursor :: Square -> Board (Widget n) -> Board (Widget n)
highlightCursor cur bd = seek cur bd&bdSel %~ withAttr cursorAttr

-- Add colors to the cells which a piece can move to.
highlightSelected :: [Square] -> Board (Widget n) -> Board (Widget n)
highlightSelected ms bd
  = foldr (((bdSel %~ withAttr selectedAttr) .) . seek) bd ms
  & seek (pos bd)

-- Add colors to the board.
colorCells :: Board (Widget n) -> Board (Widget n)
colorCells = extend colorSelected

  where colorSelected bd
          | even squareSum = modifyDefAttr (`withBackColor` rgbColor @Integer 80 80 80) (extract bd)
          | otherwise = modifyDefAttr (`withBackColor` rgbColor @Integer 210 210 210) (extract bd)

          where squareSum = fromEnum (bd^.square.file) + fromEnum (bd^.square.rank)

-- Transform chess pieces into widgets.
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

        colorWidget White = modifyDefAttr (flip withStyle bold . (`withForeColor` rgbColor @Integer 255 255 255))
        colorWidget Black = modifyDefAttr (flip withStyle bold . (`withForeColor` rgbColor @Integer 0 0 0))

chessAttrMap :: ChessGame -> AttrMap
chessAttrMap = const $ attrMap defAttr
  [ (selectedAttr, bg $ rgbColor @Integer 170 170 170)
  , (cursorAttr, bg $ rgbColor @Integer 120 120 120)
  ]

cursorAttr, selectedAttr :: AttrName
selectedAttr = attrName "selected"
cursorAttr = attrName "cursor"

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
