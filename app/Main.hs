module Main where

import Board

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center

import Control.Comonad.Store
import Control.Monad (forM_)

data Game = Game {}

newGame :: Game
newGame = undefined

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  simpleMain $ hCenter $ renderTable $ table $ map (map (padLeftRight 1)) (boardToWidgets standardChessboard)

boardToWidgets :: Chessboard -> [[Widget ()]]
boardToWidgets board = toGrid $ fmap toWidget board
  where toWidget (Just (Pawn   _)) = pawnWidget
        toWidget (Just (Queen  _)) = queenWidget
        toWidget (Just (King   _)) = kingWidget
        toWidget (Just (Rook   _)) = rookWidget
        toWidget (Just (Bishop _)) = bishopWidget
        toWidget (Just (Knight _)) = knightWidget
        toWidget Nothing = emptyCell

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
