{-# LANGUAGE LambdaCase #-}
module LambdaChess
  ( moves
  , move
  ) where

import Control.Comonad
import Control.Comonad.Store

import Control.Lens
import Control.Monad

import Data.List (unfoldr, singleton, sort)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Bool (bool)

import LambdaChess.Board

-- | Find valid moves for the currently focused piece.
moves :: Chessboard -> [[Square]]
moves bd = ($ pos bd) . flip (maybe (const [])) (extract bd) $ \case
  (Piece Pawn White) -> filterHiddenPieces . fmap maybeToList . singleton . sqUp
  (Piece Pawn Black) -> filterHiddenPieces . fmap maybeToList . singleton . sqDown
  (Piece Bishop   _) -> filterHiddenPieces . expandLines diagonalMoves
  (Piece Rook     _) -> filterHiddenPieces . expandLines straightMoves
  (Piece Queen    _) -> filterHiddenPieces . expandLines (straightMoves <> diagonalMoves)
  (Piece King     _) -> filterHiddenPieces . fmap maybeToList . sequence (straightMoves <> diagonalMoves)
  (Piece Knight   _) -> filterHiddenPieces . fmap maybeToList . sequence knightMoves

  where
    filterHiddenPieces :: [[Square]] -> [[Square]]
    filterHiddenPieces moveList = do
      -- Sort ensures correct behavior.
      (emptySquares, piece) <- map ((_2 %~ listToMaybe) . span isEmpty) (sort moveList)

      -- Can this piece be taken.
      let isTarget = (bool (const Nothing) Just =<< isRival) =<< piece
      pure $ maybe emptySquares (: emptySquares) isTarget

    extendLine f  = unfoldr (fmap (join (,)) . f) -- Extend move direction to board edges.
    expandLines   = mapM extendLine               -- Extend lines for every move direction.

    isEmpty       = isNothing . (`peek` bd) -- Is the square empty?
    isRival rival = (col <$> extract bd) /= (col <$> peek rival bd) -- Check if the given square is of the same colour.

    -- Basic move directions.
    diagonalMoves, straightMoves, knightMoves :: [Square -> Maybe Square]
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
