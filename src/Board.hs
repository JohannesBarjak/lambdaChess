{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Board
  ( Board
  , Chessboard
  , Coord(..)
  , Square(..)
  , HasSquare(..)
  , Piece(..)
  , Player(..)
  , piece
  , standardChessboard
  ) where

import Control.Comonad.Store
import Control.Lens
import Data.Bool (bool)

import Data.Function (on)
import Data.Vector (Vector)
import Data.Vector qualified as V

data Board a = Board Square (Vector (Vector a))
  deriving Functor

data Square = Square { _rank :: Coord, _file :: Coord }
  deriving (Eq, Ord)

data Coord
  = A | B
  | C | D
  | E | F
  | G | H
  deriving (Eq, Ord, Enum, Bounded)

data Player = White | Black

data Piece
  = Queen Player
  | King Player
  | Rook Player
  | Bishop Player
  | Knight Player
  | Pawn Player

type Chessboard = Board (Maybe Piece)

makeClassy ''Square

instance Comonad Board where
  extract (Board s board) = board V.! fromEnum (s^.rank) V.! fromEnum (s^.file)

  extend f (Board position board)
    = Board position do
      rk <- [A ..]
      pure $ do
        fl <- [A ..]
        pure $ f (Board (Square rk fl) board)

instance ComonadStore Square Board where
  pos (Board s _) = s
  peek s (Board _ board) = extract (Board s board)

instance Applicative Board where
  pure x = Board (Square A A) (V.replicate 8 $ V.replicate 8 x)

  (Board pos1 fboard) <*> (Board pos2 board)
    = Board truncPosSum $ V.zipWith (V.zipWith ($)) fboard board

    where truncPosSum
            = let overflowCheck a = bool (toEnum a) maxBound (a > fromEnum (maxBound @Coord)) in
                Square (overflowCheck rk) (overflowCheck fl)

              where rk = on (+) fromEnum (pos1^.rank) (pos2^.rank)
                    fl = on (+) fromEnum (pos1^.file) (pos2^.file)

instance ComonadApply Board

instance HasSquare (Board a) where
  square = lens pos (flip seek)

piece :: Lens' (Board a) a
piece = lens extract \(Board s board) a -> Board s
    $ board&singular (ix $ rk s).singular (ix $ fl s) .~ a

  where rk s = fromEnum (s^.rank)
        fl s = fromEnum (s^.file)

standardChessboard :: Chessboard
standardChessboard = extend pieces . extend pawns $ pure Nothing
  where pieces board = case pos board of
            (Square A fl) -> Just $ pieceList White !! fromEnum fl
            (Square H fl) -> Just $ pieceList Black !! fromEnum fl
            _ -> view piece board

        pawns board = case pos board of
            (Square B _) -> Just (Pawn White)
            (Square G _) -> Just (Pawn Black)
            _ -> view piece board

        pieceList col = map ($ col) [ Rook, Knight, Bishop
                                    , Queen, King
                                    , Bishop, Knight, Rook
                                    ]
