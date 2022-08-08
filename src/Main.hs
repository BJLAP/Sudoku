module Main where
import qualified GameLogic
main :: IO ()
main = do
    putStrLn "Sudoko solver"
    let resolution = GameLogic.solve GameLogic.inputGrid
    print resolution
