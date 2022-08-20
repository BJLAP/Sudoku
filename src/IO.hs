module IO (getValues, displayGrid) where

import GameLogic
import Control.Monad.Reader

globalGridSize = return 9 :: Reader String Int

getValues :: Grid -> [[Value]]
getValues = map (map getValue)

addSeparator :: [Value] -> [String]
addSeparator = map ((" | " ++) . show)
--map ((" " ++) . show) [1, 2, 3]

--map (("|" ++) . show) [1, 2, 3]

displayLine :: [String] -> IO ()
displayLine line = mapM_ putStr line >> putStr " |" >> putStrLn ""

displayGrid :: Grid -> IO ()
displayGrid grid = putStrLn " -----------------" >> mapM_ (displayLine . addSeparator) (getValues grid) >> putStrLn " -------------"

test :: a -> IO ()
test _ = putStr "Hello" >> putStrLn " World" 
