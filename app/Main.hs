module Main where

import qualified LambdaChess (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  LambdaChess.someFunc
