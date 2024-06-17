{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module LambdaChess
  ( Square
  , Coord(..)
  , HasSquare(..)
  , Piece(..)
  , Player(..)
  , Board
  , ChessBoard
  ) where

import Control.Comonad.Store
import Control.Lens

import Data.Vector (Vector)
import Data.Vector qualified as V

data Square = Square { _rank :: Coord, _file :: Coord }
  deriving (Eq, Ord)

data Coord
  = A | B
  | C | D
  | E | F
  | G | H
  deriving (Eq, Ord, Enum)

data Player = White | Black

data Piece
  = Queen Player
  | King Player
  | Rook Player
  | Bishop Player
  | Knight Player
  | Pawn Player

data Board a = Board Square (Vector (Vector a))
  deriving Functor

type ChessBoard = Board (Maybe Piece)

makeClassy ''Square

instance Comonad Board where
  extract (Board s board) = board V.! fromEnum (s^.rank) V.! fromEnum (s^.file)

  extend f (Board position board) = Board position $ do
    rk <- [A ..]
    pure $ do
      fl <- [A ..]
      pure $ f (Board (Square rk fl) board)

instance ComonadStore Square Board where
  pos (Board s _) = s
  peek s (Board _ board) = extract (Board s board)
