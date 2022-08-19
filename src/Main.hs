module Main where
import GameLogic
main :: IO ()
main = do
    putStrLn "Sudoko solver"
    let resolution = solve inputGrid
    print resolution
