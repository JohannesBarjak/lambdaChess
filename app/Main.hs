module Main where

import LambdaChess
import Data.Map qualified as Map

main :: IO ()
main = print $ showBoard standardBoard

showBoard :: Board -> [String]
showBoard board = do
  rows <- enumFrom A
  pure $ do
    cols <- enumFrom A

    let piece = Map.lookup (Square rows cols) board
    pure $ maybe ' ' pieceToChar piece

pieceToChar :: Piece -> Char
pieceToChar (Piece King _)   = 'K'
pieceToChar (Piece Queen _)  = 'Q'
pieceToChar (Piece Rook _)   = 'R'
pieceToChar (Piece Bishop _) = 'B'
pieceToChar (Piece Knight _) = 'N'
pieceToChar (Piece Pawn _)   = 'P'
