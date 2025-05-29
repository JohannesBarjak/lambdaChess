{-# LANGUAGE LambdaCase #-}
module LambdaChess
  ( moves
  , move
  ) where

import Control.Comonad
import Control.Comonad.Store

import Control.Lens
import Control.Monad

import Data.List (unfoldr, singleton)
import Data.Maybe (catMaybes, isNothing, listToMaybe, maybeToList)
import Data.Bool (bool)

import LambdaChess.Board

moves :: Chessboard -> [Square]
moves bd = ($ pos bd) . flip (maybe (const [])) (extract bd) $ \case
  (Piece Pawn White) -> catMaybes . singleton . sqUp
  (Piece Pawn Black) -> catMaybes . singleton . sqDown
  (Piece Bishop   _) -> concat . expandLines diagonalMoves
  (Piece Rook     _) -> do
    extendedMoves <- expandLines straightMoves

    pure do
      (emptySquares, piece) <- map ((_2 %~ listToMaybe) . span isEmpty) extendedMoves

      let isTarget = maybe False isRival piece
      emptySquares ++ bool [] (maybeToList piece) isTarget

  (Piece Queen    _) -> concat . expandLines (straightMoves <> diagonalMoves)
  (Piece King     _) -> catMaybes . sequence (straightMoves <> diagonalMoves)
  (Piece Knight   _) -> catMaybes . sequence knightMoves

  where
    extendLine f  = unfoldr (fmap (join (,)) . f) -- Extend move direction to board edges.
    expandLines   = mapM extendLine               -- Extend lines for every move direction.

    isEmpty       = isNothing . (`peek` bd) -- Is the square empty?
    isRival rival = (col <$> extract bd) /= (col <$> peek rival bd) -- Check if the given square is of the same colour.

    -- Basic move directions.
    diagonalMoves = [sqUp >=> sqRight, sqUp >=> sqLeft, sqDown >=> sqRight, sqDown >=> sqLeft]
    straightMoves = [sqUp, sqDown, sqLeft, sqRight]

    knightMoves   =
      let twice f = f >=> f in
        [ twice sqUp    >=> sqLeft, twice sqUp    >=> sqRight
        , twice sqDown  >=> sqLeft, twice sqDown  >=> sqRight
        , twice sqLeft  >=> sqDown, twice sqLeft  >=> sqUp
        , twice sqRight >=> sqDown, twice sqRight >=> sqUp
        ]

move :: Square -> Chessboard -> Chessboard
move target bd = seek target bd&bdSel .~ extract bd&seek (pos bd)&bdSel .~ Nothing
