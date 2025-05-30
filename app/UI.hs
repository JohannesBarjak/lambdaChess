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
import Control.Monad.State.Strict

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
  . bool id (highlightSelected (concat $ moves $ gState^.board)) (gState^.selected)
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

    -- Vim style movement.
    KChar c | toLower c == 'h' -> cursorMove sqLeft
    KChar c | toLower c == 'j' -> cursorMove sqDown
    KChar c | toLower c == 'k' -> cursorMove sqUp
    KChar c | toLower c == 'l' -> cursorMove sqRight

    -- Also add movement using the arrow keys.
    KUp    -> cursorMove sqUp
    KDown  -> cursorMove sqDown
    KLeft  -> cursorMove sqLeft
    KRight -> cursorMove sqRight

    KEnter -> do
      bd    <- use board
      color <- use turn
      cur   <- use cursor
      sel   <- use selected

      -- Move when user chooses a valid move and then switch turn.
      when (sel && (cur `elem` concat (moves bd))) do
        board %= move cur
        turn %= otherPlayer

      -- Check if the selected square has a friendly piece.
      let isPlayerPiece = maybe False (\piece -> col piece == color) (peek cur bd)

      -- Only select friendly pieces.
      when isPlayerPiece $ board %= seek cur
      selected .= isPlayerPiece

      -- Disable selection when pressing already selected piece.
      when (sel && (cur == pos bd)) (assign selected False)

    -- Escape selection.
    KEsc -> selected .= False

    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

-- Move the cursor according to the function given.
cursorMove :: (MonadState s m, HasChessGame s, Foldable t) => (Square -> t Square) -> m ()
cursorMove f = traverse_ (assign cursor) . f  =<< use cursor

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
