module LambdaChess
  ( moves
  , move
  ) where

import Control.Comonad
import Control.Comonad.Store

import Control.Lens
import Control.Monad
import Data.Function

import Data.List (unfoldr, singleton, sort)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Bool (bool)

import LambdaChess.Board
import LambdaChess.Utils

-- | Find valid moves for the currently focused piece.
moves :: Chessboard -> [[Square]]
moves = filterHiddenPieces <*< (flip id . pos) <*> maybe (const []) constructMoves . extract

  where
    constructMoves :: PlayerPiece -> Square -> [[Square]]
    constructMoves (Piece Pawn White) = fmap maybeToList . singleton .  sqUp
    constructMoves (Piece Pawn Black) = fmap maybeToList . singleton . sqDown
    constructMoves (Piece Bishop   _) = expandLines diagonalMoves
    constructMoves (Piece Rook     _) = expandLines straightMoves
    constructMoves (Piece Queen    _) = expandLines (straightMoves <> diagonalMoves)
    constructMoves (Piece King     _) = fmap maybeToList . sequence (straightMoves <> diagonalMoves)
    constructMoves (Piece Knight   _) = fmap maybeToList . sequence knightMoves

    filterHiddenPieces :: Chessboard -> [[Square]] -> [[Square]]
    filterHiddenPieces bd moveList = do
      -- Sort ensures correct behavior.
      (emptySquares, piece) <- over _2 listToMaybe . span (isEmpty bd) <$> sort moveList

      -- Can this piece be taken.
      let isTarget = (bool (const Nothing) Just =<< isRival bd) =<< piece

      pure $ maybe emptySquares (: emptySquares) isTarget

    extendLine  = unfoldr . (fmap (join (,)) .) -- Extend move direction to board edges.
    expandLines = mapM extendLine               -- Extend lines for every move direction.

    isEmpty = isNothing .: flip peek -- Is the square empty?
    isRival = flip (liftA2 ((/=) `on` fmap col) extract) .^ peek -- Check if the given square is of the same colour.

    -- Basic move directions.
    diagonalMoves, straightMoves, knightMoves :: [Square -> Maybe Square]
    diagonalMoves = [sqUp >=> sqRight, sqUp >=> sqLeft, sqDown >=> sqRight, sqDown >=> sqLeft]
    straightMoves = [sqUp, sqDown, sqLeft, sqRight]

    knightMoves =
      let twice = join (>=>) in
        [ twice sqUp    >=> sqLeft, twice sqUp    >=> sqRight
        , twice sqDown  >=> sqLeft, twice sqDown  >=> sqRight
        , twice sqLeft  >=> sqDown, twice sqLeft  >=> sqUp
        , twice sqRight >=> sqDown, twice sqRight >=> sqUp
        ]

move :: Square -> Chessboard -> Chessboard
move = (set bdSel Nothing .: (liftA2 seek pos . liftA2 (bdSel .~) extract)) . seek
