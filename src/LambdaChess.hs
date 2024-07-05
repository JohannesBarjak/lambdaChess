{-# LANGUAGE LambdaCase #-}
module LambdaChess
  ( moves
  , move
  ) where

import Control.Comonad
import Control.Comonad.Store

import Control.Lens
import Control.Monad

import Data.List (unfoldr)
import Data.Maybe (catMaybes)

import LambdaChess.Board

moves :: Chessboard -> [Square]
moves bd = flip (maybe []) (extract bd) \case
  (Piece Pawn White) -> catMaybes [sqUp (pos bd)]
  (Piece Pawn Black) -> catMaybes [sqDown (pos bd)]
  (Piece Bishop   _) -> concat . mapM boardLine bishopMoves $ pos bd
  _ -> []

  where boardLine f = unfoldr (fmap (join (,)) . f)

        bishopMoves = [sqUp >=> sqRight, sqUp >=> sqLeft, sqDown >=> sqRight, sqDown >=> sqLeft]

move :: Square -> Chessboard -> Chessboard
move target bd = seek target bd&bdSel .~ extract bd&seek (pos bd)&bdSel .~ Nothing
