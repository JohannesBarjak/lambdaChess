module Main where

import Brick
import Control.Monad (void)

import UI

main :: IO ()
main = void $ defaultMain initialApp newGame
