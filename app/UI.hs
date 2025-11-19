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
import Control.Monad.State.Strict
import Control.Arrow

import Data.Foldable (traverse_)

import Data.Bool (bool)
import Data.Char (toLower)
import Graphics.Vty

import LambdaChess
import LambdaChess.Board
import LambdaChess.Utils

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

-- | Main game pipeline.
draw :: ChessGame -> [Widget n]
draw gState =
  [ renderWidgetBoard
  . (bool id <$> highlightMoves . concat . moves . (^.board) <*> (^.selected) $ gState)
  . highlightCursor (gState^.cursor)
  . colorCells
  . widgetBoard $ gState^.board
  ]

-- | Final board image to draw.
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

    KEnter -> manageBoard
    -- Escape selection.
    KEsc -> selected .= False

    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

manageBoard :: EventM n ChessGame ()
manageBoard = do
  sel   <- use selected
  bd    <- use board
  ptr   <- use cursor
  color <- use turn

  -- Check if the selected square is of the same color as the current player
  let isPlayer = player color ptr bd

  -- Move when user chooses a valid move and then switch player.
  let isValidMove = validMove sel ptr bd

  turn  %= updateIf isValidMove otherPlayer
  board %= updateIf isPlayer (seek ptr)
  board %= updateIf isValidMove (move ptr)

  -- Only select friendly pieces.
  -- Disable selection when pressing already selected piece.
  selected .= bool isPlayer False (sel && ptr == pos bd)

  where player    = (.: peek) . maybe False . ((==) .^ col)
        updateIf  = flip $ bool id
        validMove = (.: (. concat . moves) . elem) . (&&)

-- | Move the cursor according to the function given.
cursorMove :: (MonadState s m, HasChessGame s, Foldable t) => (Square -> t Square) -> m ()
cursorMove = (use cursor >>=) . (traverse_ (assign cursor) .)

-- | Invert player chess color.
otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

highlightCursor :: Square -> Board (Widget n) -> Board (Widget n)
highlightCursor = over bdSel (withAttr cursorAttr) .: seek

-- | Add move colors to the cells which a piece can move to.
highlightMoves :: [Square] -> Board (Widget n) -> Board (Widget n)
highlightMoves = (seek . pos <*>) . flip (foldr $ over bdSel (withAttr selectedAttr) .: seek)

-- | Add alternating colors to the board.
colorCells :: Board (Widget n) -> Board (Widget n)
colorCells = extend colorSelected

  where colorSelected bd
          | even squareSum = modifyDefAttr (`withBackColor` rgbColor @Integer 80 80 80) (extract bd)
          | otherwise      = modifyDefAttr (`withBackColor` rgbColor @Integer 210 210 210) (extract bd)

          where squareSum = fromEnum (bd^.square.file) + fromEnum (bd^.square.rank)

-- | Transform chess pieces into widgets.
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

        colorWidget White = modifyDefAttr
          (flip withStyle bold . (`withForeColor` rgbColor @Integer 255 255 255))
        colorWidget Black = modifyDefAttr
          (flip withStyle bold . (`withForeColor` rgbColor @Integer 0 0 0))

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
