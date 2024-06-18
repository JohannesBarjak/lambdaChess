module Main where

import Brick
import LambdaChess

data Game = Game {}

newGame :: Game
newGame = undefined

ui :: Widget ()
ui = str "Hello World!"

main :: IO ()
main = do
  simpleMain ui
