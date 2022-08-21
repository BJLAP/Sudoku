module Main where
import IO
import GameLogic
import Control.Monad.Reader
main :: IO ()
main = do
    putStrLn "Welcome to Sudoko Solver!"

    -- Read input file
    putStrLn "Enter an input file name:"
    fileName <- getLine
    inputFromFile <- readValuesFromFile fileName

    -- Convert input to grid 
    let grid = createGrid inputFromFile 0
    putStrLn "Input:"
    displayGrid grid

    -- Calculate solution
    putStrLn "Solution:"
    let resolution = runReader solveM (createGrid inputFromFile 0)
    displayGrid resolution
