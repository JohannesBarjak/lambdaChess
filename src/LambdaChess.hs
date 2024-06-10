module LambdaChess
  ( Piece(..)
  , PieceType(..)
  , Color(..)
  , Square(..)
  , Coord(..)
  , Board
  , standardBoard
  ) where

import Data.Map qualified as Map
import Data.Map (Map)

type Board = Map Square Piece
data Square = Square {-# UNPACK #-} !Coord !Coord
  deriving (Eq, Ord)

data Piece = Piece PieceType Color

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

data Coord
  = A | B
  | C | D
  | E | F
  | G | H
  deriving (Eq, Ord, Enum)

data Color = White | Black

standardBoard :: Board
standardBoard = Map.fromList
  $  standardPlacement White A
  ++ pawnRow White B
  ++ standardPlacement Black H
  ++ pawnRow Black G

standardPlacement :: Color -> Coord -> [(Square, Piece)]
standardPlacement col coord = zip coordinates pieces
  where coordinates = map (Square coord) (enumFrom A)

        pieces = map (`Piece` col) (leftPieces ++ [Queen, King] ++ reverse leftPieces)
        leftPieces = [Rook, Knight, Bishop]

pawnRow :: Color -> Coord -> [(Square, Piece)]
pawnRow c coord = map ((, Piece Pawn c) . Square coord) (enumFrom A)
