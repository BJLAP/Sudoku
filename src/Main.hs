module Main where
import IO
import GameLogic
import Control.Monad.Reader
main :: IO ()
main = do
    putStrLn "Welcome to Sudoko Solver!"
    -- todo: read inputGrid from CSV
    let resolution = runReader solveM inputGrid
    displayGrid resolution
