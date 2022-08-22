module IO (displayGrid, readValuesFromFile) where

import GameLogic
import Data.List.Split
import Control.Exception

addSeparator :: [Value] -> [String]
addSeparator = map ((" | " ++) . show)

displayLine :: [String] -> IO ()
displayLine line = mapM_ putStr line >> putStr " |" >> putStrLn ""

displayGrid :: Grid -> IO ()
displayGrid grid = putStrLn " -------------------------------------" >> mapM_ (displayLine . addSeparator) (getValues grid) >> putStrLn " -------------------------------------"

readLines :: FilePath -> IO [String]
readLines = fmap (tail . lines) . readFile

readValuesFromFile :: String -> IO [[Int]]
readValuesFromFile fileName = do
    text <- readLines fileName
    let convertedValues = map (splitOn ";") text
    let convertedInts = map (map (read::String->Int)) convertedValues
    return convertedInts

--tryReadFile :: FilePath -> IO()
--tryReadFile filePath = do 
--    eitherErrorOrFile <- try (readFile filePath)
--    case eitherErrorOrFile of
--        Left _ -> print "error"
--        Right content -> print content

