{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module LambdaChess.Board
  ( Board
  , Chessboard
  , Coord(..)
  , Square(..)
  , HasSquare(..)
  , Piece(..)
  , Player(..)
  , PlayerPiece(..)
  , bdSel
  , toGrid
  , standardChessboard
  , sqUp, sqDown, sqLeft, sqRight
  ) where

import Control.Comonad.Store
import Control.Lens

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

data PlayerPiece = Piece
  { piece :: Piece
  , col :: Player
  }

data Piece
  = Queen
  | King
  | Rook
  | Bishop
  | Knight
  | Pawn

type Chessboard = Board (Maybe PlayerPiece)

makeClassy ''Square

instance Comonad Board where
  extract (Board s board) = board V.! fromEnum (s^.rank) V.! fromEnum (s^.file)

  extend f (Board position board)
    = Board position do
      rk <- [A ..]
      pure do
        fl <- [A ..]
        pure $ f (Board (Square rk fl) board)

instance ComonadStore Square Board where
  pos (Board s _) = s
  peek s (Board _ board) = extract (Board s board)

instance Applicative Board where
  pure x = Board (Square A A) (V.replicate 8 $ V.replicate 8 x)

  (Board pos1 fboard) <*> (Board pos2 board)
    = Board truncPosSum $ V.zipWith (V.zipWith ($)) fboard board

    where truncPosSum = Square (boundedMove rk) (boundedMove fl)

              where rk = on (+) fromEnum (pos1^.rank) (pos2^.rank)
                    fl = on (+) fromEnum (pos1^.file) (pos2^.file)

instance ComonadApply Board

instance HasSquare (Board a) where
  square = lens pos (flip seek)

sqUp, sqDown, sqLeft, sqRight :: Square -> Square
sqUp    s = s&rank %~ boundedMove . (+1) . fromEnum
sqDown  s = s&rank %~ boundedMove . subtract 1 . fromEnum
sqLeft  s = s&file %~ boundedMove . subtract 1 . fromEnum
sqRight s = s&file %~ boundedMove . (+1) . fromEnum

boundedMove :: Int -> Coord
boundedMove c
  | c > fromEnum (maxBound @Coord) = maxBound
  | c < fromEnum (minBound @Coord) = minBound
  | otherwise = toEnum c

bdSel :: Lens' (Board a) a
bdSel = lens extract \(Board s board) a -> Board s
    $ board&singular (ix $ rk s).singular (ix $ fl s) .~ a

  where rk s = fromEnum (s^.rank)
        fl s = fromEnum (s^.file)

toGrid :: Board a -> [[a]]
toGrid board = do
  rk <- [A ..]

  pure do
    fl <- [A ..]
    pure $ peek (Square rk fl) board

standardChessboard :: Chessboard
standardChessboard = extend pieces . extend pawns $ pure Nothing
  where pieces board = case pos board of
            (Square A fl) -> Just $ pieceList White !! fromEnum fl
            (Square H fl) -> Just $ pieceList Black !! fromEnum fl
            _ -> view bdSel board

        pawns board = case pos board of
            (Square B _) -> Just (Piece Pawn White)
            (Square G _) -> Just (Piece Pawn Black)
            _ -> view bdSel board

        pieceList col
          = map (`Piece` col)
            [ Rook, Knight, Bishop
            , Queen, King
            , Bishop, Knight, Rook
            ]
