{-# LANGUAGE LambdaCase #-}
module LambdaChess
  ( Move
  , moves
  , move
  , getMove
  ) where

import LambdaChess.Board

import Control.Comonad
import Control.Comonad.Store

import Control.Lens

newtype Move = Move Square

moves :: Chessboard -> [Move]
moves bd = flip (maybe []) (extract bd) \case
  (Piece Pawn White) -> map (Move . sqUp) [pos bd, sqUp (pos bd)]
  _ -> []

move :: Move -> Chessboard -> Chessboard
move (Move target) bd = seek target bd&bdSel .~ extract bd&seek (pos bd)&bdSel .~ Nothing

getMove :: Move -> Square
getMove (Move s) = s
