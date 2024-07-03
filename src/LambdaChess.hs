{-# LANGUAGE LambdaCase #-}
module LambdaChess
  ( moves
  , move
  ) where

import LambdaChess.Board

import Control.Comonad
import Control.Comonad.Store

import Control.Lens

moves :: Chessboard -> [Square]
moves bd = flip (maybe []) (extract bd) \case
  (Piece Pawn White) -> map sqUp [pos bd, sqUp (pos bd)]
  _ -> []

move :: Square -> Chessboard -> Chessboard
move target bd = seek target bd&bdSel .~ extract bd&seek (pos bd)&bdSel .~ Nothing
