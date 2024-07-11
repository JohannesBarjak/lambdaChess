{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
module UI where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Table

import Control.Comonad.Store
import Control.Lens
import Control.Monad
import Control.Arrow ((>>>))

import Data.Char (toLower)
import Data.Foldable (for_)

import Graphics.Vty

import LambdaChess
import LambdaChess.Board

data ChessGame = ChessGame
  { _cursor :: Square
  , _board :: Chessboard
  , _selected :: Bool
  }

makeLenses ''ChessGame

newGame :: ChessGame
newGame = ChessGame
  { _cursor = Square A A
  , _board = standardChessboard
  , _selected = False
  }

initialApp :: App ChessGame e ()
initialApp = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = chessAttrMap
  }

draw :: ChessGame -> [Widget n]
draw gState =
  [ vCenter . hCenter
  $ renderWidgetBoard
  $ highlightSelected (gState^.selected) (moves $ gState^.board)
  $ highlightCursor (gState^.cursor)
  $ colorCells
  $ widgetBoard (gState^.board)
  ]

renderWidgetBoard :: Board (Widget n) -> Widget n
renderWidgetBoard
  = renderTable
  . columnBorders False . rowBorders False
  . table . reverse . toGrid

handleEvent :: BrickEvent n e -> EventM n ChessGame ()
handleEvent = \case
  (VtyEvent (EvKey key _)) -> case key of
    KChar c | toLower c == 'q' -> halt

    KUp -> maybe (pure ()) (cursor .=) . sqUp =<< use cursor
    KDown -> maybe (pure ()) (cursor .=) . sqDown =<< use cursor
    KLeft -> maybe (pure ()) (cursor .=) . sqLeft =<< use cursor
    KRight -> maybe (pure ()) (cursor .=) . sqRight =<< use cursor

    KEnter -> do
      sel <- use selected
      cur <- use cursor
      bd <- use board

      if sel then when (cur `elem` moves bd) do
            board %= move cur
            selected .= False

        else for_ (peek cur bd) . const $
              unless (null $ moves (seek cur bd)) do
                board %= seek cur
                selected .= True

    _ -> continueWithoutRedraw
  _ -> continueWithoutRedraw

highlightCursor :: Square -> Board (Widget n) -> Board (Widget n)
highlightCursor cur bd = seek cur bd&bdSel %~ withAttr cursorAttr

highlightSelected :: Bool -> [Square] -> Board (Widget n) -> Board (Widget n)
highlightSelected False _  bd = bd
highlightSelected True  ms bd
  = foldr (((bdSel %~ withAttr selectedAttr) .) . seek) bd ms
  & seek (pos bd)

colorCells :: Board (Widget n) -> Board (Widget n)
colorCells = extend colorSelected

  where colorSelected bd
          | even squareSum = modifyDefAttr (`withBackColor` rgbColor @Integer 80 80 80) (extract bd)
          | otherwise = modifyDefAttr (`withBackColor` rgbColor @Integer 210 210 210) (extract bd)

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
