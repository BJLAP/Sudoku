module GameLogic(solveM, createGrid, getValues, Grid, Field, Value) where

import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad.Reader

type Value = Int
type HorPos = Int
type VerPos = Int
type ExcludedValues = [Value]
type Location = (HorPos, VerPos)
type Row = [Field]
type InputValues = [[Value]]
type Grid = [[Field]]
type Field = (Location, Value, ExcludedValues)

gridSize = 9

solveM :: Reader Grid Grid
solveM = do
    inputGame <- ask
    let result = solve inputGame
    return result

solve :: Grid -> Grid
solve grid | isComplete grid = grid
solve grid = if grid /= updatedGrid then solve updatedGrid else grid
    where updatedGrid = updateGrid grid (findNextValues grid)

createGrid :: InputValues -> Int -> Grid
createGrid [] _ = []
createGrid (x:xs) init | init < gridSize = createTuples x init 0 : createGrid xs (init + 1)
createGrid _ _ = []

createTuples :: [Value] -> HorPos -> VerPos -> [Field]
createTuples [] _ _ = []
createTuples (x:xs) xpos ypos | ypos < gridSize = ((xpos, ypos), x, []) : createTuples xs xpos (ypos + 1)
createTuples _ _ _ = []

updateGrid :: Grid -> [Maybe Field] -> Grid
updateGrid grid fields = [updateRow row fields | row <- grid]

updateRow :: Row -> [Maybe Field] -> Row
updateRow row fields  = [ updateField fields fieldToUpdate | fieldToUpdate <- row]

updateField :: [Maybe Field] -> Field -> Field
updateField [] field = field
updateField (Just x:xs) field = if getLocation x == getLocation field then x else updateField xs field
updateField (Nothing:xs) field = updateField xs field


isComplete :: Grid -> Bool
isComplete grid = foldr ((&&).(>0).getValue) True (concat grid)

findNextValues :: Grid -> [Maybe Field]
findNextValues grid = map (findValue gridWithExcludedValues) (concat gridWithExcludedValues) -- use map with partial function
    where gridWithExcludedValues = calcExcludedValues grid

findValue :: Grid -> Field -> Maybe Field
findValue grid field | getValue field == 0 = determineValueGrid grid field 1
findValue _ _ = Nothing

groupSquares :: [[a]] -> [[a]]
groupSquares [[]] = []
groupSquares grid = concatMap (map concat . transpose) (chunksOf (getSqrt gridSize) (map (chunksOf (getSqrt gridSize)) grid))

getSqrt :: Int -> Int
getSqrt x = round(sqrt (fromIntegral x))

findRelatedFields :: Field -> Grid -> [Field]
findRelatedFields field grid = concatMap (listContainsField field) grid ++ concatMap (listContainsField field) (transpose grid) ++  concatMap (listContainsField field) (groupSquares grid)

listContainsField :: Field -> [Field] -> [Field]
listContainsField field fields = if foldr ((||).(== getLocation field).getLocation)False fields then fields else []

determineValueGrid :: Grid -> Field -> Int -> Maybe Field
determineValueGrid [[]] _ _ = Nothing
determineValueGrid grid field y | isJust maybeValueRow = maybeValueRow
                                | isJust maybeValueVerticalRow = maybeValueVerticalRow
                                | isJust maybeValueSquare = maybeValueSquare
                                | otherwise = Nothing
    where maybeValueRow = determineValueRow field y (getRow field grid)
          maybeValueVerticalRow = determineValueRow field y (getRow field (transpose grid))
          maybeValueSquare = determineValueRow field y (getRow field (groupSquares grid))

determineValueRow :: Field -> Int -> Row -> Maybe Field
determineValueRow _ _ [] = Nothing
-- Strategy 1 for solving Sudoku: if all possible values but 1 are excluded for a field
-- (because related fields contain these values), there is only one possible value
determineValueRow field y row | y > 0  && y <= gridSize && length(getExcludedValues field) == (gridSize -1 ) = if y `notElem` getExcludedValues field 
    then Just (getLocation field, y, getExcludedValues field) 
    else determineValueRow field (y + 1) row
-- Strategy 2 for solving Sudoku: if a value is allowed in a field but it is excluded
-- in all related empty fields (that is empty fields in square or horizontal row or vertical row) than this must be the valid value for this field
determineValueRow field y row | y > 0  && y <= gridSize = if (y `notElem` getExcludedValues field) && valueIsExcludedInRow y (removeValue field row) 
    then Just (getLocation field, y, getExcludedValues field) 
    else determineValueRow field (y + 1) row
determineValueRow _ _ _  = Nothing

valueIsExcludedInRow :: Int -> [Field] -> Bool 
valueIsExcludedInRow _ [] = False
valueIsExcludedInRow x xs =  all (elem x . getExcludedValues) (filter (\y -> getValue y == 0) xs)

removeValue :: (Eq a) => a -> [a] -> [a]
removeValue _ []              = []
removeValue x (y:ys) | x == y = removeValue x ys
                     | otherwise  = y : removeValue x ys

getLocation :: Field -> Location
getLocation (l,_,_) = l

getValue :: Field -> Value
getValue (_,v,_) = v

getValues :: Grid -> [[Value]]
getValues = map (map getValue)

getExcludedValues :: Field -> ExcludedValues
getExcludedValues (_,_,e) = e

calcExcludedValues :: Grid -> Grid
calcExcludedValues grid = map (calcExcludedValuesRow grid) grid 

calcExcludedValuesRow :: Grid -> Row -> Row
calcExcludedValuesRow grid = map (calcExcludedValuesField grid)

calcExcludedValuesField :: Grid -> Field -> Field
calcExcludedValuesField grid field = (getLocation field, getValue field, removeValue (getValue field) (getDistinctValues(findRelatedFields field grid)))

getDistinctValues :: [Field] -> [Int]
getDistinctValues fields = removeValue 0 (nub(map getValue fields))

getRow :: Field -> [Row] -> Row 
getRow _ [] = []
getRow field (r:rs) = if field `elem` r then r else getRow field rs 

--- Todo: remove below lines (used for testing)

test = concat (updateGrid (createGrid input 0)  [Just ((0,0), 3,[])])

inputGridSmall = calcExcludedValues (createGrid input 0)

field1 = ((0,2),0,[7,9,3,5,2,8]) :: Field
inputGrid = calcExcludedValues (createGrid inputvolk 0)
testRow = getRow field1 inputGrid
testSquare = getRow field1 (groupSquares inputGrid)
testVertical = getRow field1 (transpose inputGrid)


input = [[1,0,3,0], [2,0,0,0],[0,0,0,0],[4,0,1,0]] :: InputValues
input9by9 =[[7,9,0,0,0,3,0,0,0],[0,0,0,0,6,0,0,0,0],[8,0,0,3,0,0,0,7,6],[0,0,0,0,5,0,0,0,2],[0,0,5,4,1,8,0,0,0],[4,0,0,7,0,0,0,0,0],[6,1,0,0,9,0,0,0,8],[0,0,2,3,0,0,0,0,0],[0,0,9,0,0,0,0,5,4]] :: InputValues
inputvolk =[[6,9,0,0,0,0,3,2,0],[3,0,0,6,8,0,0,0,7],[0,2,0,9,0,7,5,0,0],[4,8,0,0,9,1,0,0,0],[0,5,1,0,0,0,9,3,0],[0,0,0,4,7,0,0,1,5],[0,0,8,2,0,9,0,5,0],[2,0,0,0,4,5,0,0,6],[0,3,6,0,0,0,0,4,9]] :: InputValues


