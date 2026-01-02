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

import Data.Vector (Vector)
import Data.Vector qualified as V

import LambdaChess.Utils

data Board a = Board Square (Vector (Vector a))
  deriving (Functor, Foldable, Traversable)

data Square = Square { _rank :: Coord, _file :: Coord }
  deriving (Eq, Ord)

data Coord
  = A | B
  | C | D
  | E | F
  | G | H
  deriving (Eq, Ord, Enum, Bounded)

data Player = White | Black deriving Eq

data PlayerPiece = Piece
  { piece :: Piece
  , col :: Player
  } deriving Eq

data Piece
  = Queen
  | King
  | Rook
  | Bishop
  | Knight
  | Pawn deriving Eq

type Chessboard = Board (Maybe PlayerPiece)

makeClassy ''Square
makeClassyPrisms ''Piece
makeClassyPrisms ''Player
makeClassyPrisms ''Coord

instance Comonad Board where
  extract (Board s board) = board V.! fromEnum (s^.rank) V.! fromEnum (s^.file)

  extend f (Board position board)
    = Board position $ (f . (`Board` board)) .: ((<$> [A ..]) . Square <$> [A ..])

instance ComonadStore Square Board where
  pos (Board s _) = s
  peek s (Board _ board) = extract (Board s board)

instance HasSquare (Board a) where
  square = lens pos (flip seek)

-- | Directional move functions.
sqUp, sqDown, sqLeft, sqRight :: Square -> Maybe Square
sqUp    = rank %%~ move . (+1) . fromEnum
sqDown  = rank %%~ move . subtract 1 . fromEnum
sqLeft  = file %%~ move . subtract 1 . fromEnum
sqRight = file %%~ move . (+1) . fromEnum

-- | Converts integers into board coordinates.
move :: Int -> Maybe Coord
move c
  | c > fromEnum (maxBound @Coord) = Nothing
  | c < fromEnum (minBound @Coord) = Nothing
  | otherwise = Just $ toEnum c

-- | Lens to selected board piece.
bdSel :: Lens' (Board a) a
bdSel = lens extract \(Board s board) a -> Board s
    $ board&singular (ix $ rk s).singular (ix $ fl s) .~ a

  where rk = fromEnum . (^.rank)
        fl = fromEnum . (^.file)

-- Converts board into a 2d list matrix.
toGrid :: Board a -> [[a]]
toGrid board = (`peek` board) .: ((<$> [A ..]) . Square) <$> [A ..]

-- | Empty board.
emptyChessboard :: Chessboard
emptyChessboard = Board (Square A A) (V.replicate 8 . V.replicate 8 $ Nothing)

-- | Board with standard chess positions.
standardChessboard :: Chessboard
standardChessboard = extend pieces . extend pawns $ emptyChessboard
  where pieces board = case pos board of
            (Square A fl) -> Just $ pieceList White !! fromEnum fl
            (Square H fl) -> Just $ pieceList Black !! fromEnum fl
            _ -> view bdSel board

        pawns board = case pos board of
            (Square B _) -> Just $ Piece Pawn White
            (Square G _) -> Just $ Piece Pawn Black
            _ -> view bdSel board

        pieceList col
          = flip Piece col <$>
            [ Rook, Knight, Bishop
            , Queen, King
            , Bishop, Knight, Rook
            ]
