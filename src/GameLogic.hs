module GameLogic(solve,inputGrid,createGrid) where

import Data.List.Split
import Data.List
import Data.Maybe

type Value = Int
type HorPos = Int
type VerPos = Int
type ExcludedValues = [Value]
type Location = (HorPos, VerPos)
type Row = [Field]
type InputValues = [[Value]]
type Grid = [[Field]]
type Field = (Location, Value, ExcludedValues)

input = [[1,0,3,0], [2,0,0,0],[0,0,0,0],[4,0,1,0]] :: InputValues
input9by9 =[[7,9,0,0,0,3,0,0,0],[0,0,0,0,6,0,0,0,0],[8,0,0,3,0,0,0,7,6],[0,0,0,0,5,0,0,0,2],[0,0,5,4,1,8,0,0,0],[4,0,0,7,0,0,0,0,0],[6,1,0,0,9,0,0,0,8],[0,0,2,3,0,0,0,0,0],[0,0,9,0,0,0,0,5,4]] :: InputValues

gridSize = 4

field1 = ((0,1),1,[1,3,2]) :: Field
field2 = ((0,3),0,[1,3]) :: Field
inputGrid = calcExcludedValues (createGrid input 0)
testRow = getRow field2 inputGrid

solve :: Grid -> Grid
solve grid | isComplete grid = grid
solve grid = if grid /= updatedGrid then solve updatedGrid else grid
    where updatedGrid = updateGrid grid (findNextValues grid)

test = concat (updateGrid (createGrid input 0)  [Just ((0,0), 3,[])])

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
--findValue grid field | getValue field == 0 = determineValue field (findRelatedFields field grid) 1
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

determineValue :: Field -> [Field] -> Int -> Maybe Field
determineValue _ [] _ = Nothing
determineValue x xs y | y > 0 && y <= gridSize = if isValidValue y xs then Just (getLocation x, y, getExcludedValues x) else determineValue x xs (y + 1)
determineValue _ _ _ = Nothing

-- use excluded values arrays of all related fields (use grid instead of [Field])
-- Todo: correct calcExcludedValues for Fields that have a value (ie. test with calcExcludedValues inputGrid
-- Expected: a field with a value has as excluded values all other values.
determineValueGrid :: Grid -> Field -> Int -> Maybe Field
determineValueGrid [[]] _ _ = Nothing
determineValueGrid grid field y | isJust maybeValueRow = maybeValueRow
                                | isJust maybeValueVerticalRow = maybeValueVerticalRow
                                | isJust maybeValueSquare = maybeValueSquare
                                | otherwise = Nothing
    where maybeValueRow = determineValueRow field y (getRow field grid)
          maybeValueVerticalRow = determineValueRow field y (getRow field (transpose grid))
          maybeValueSquare = determineValueRow field y (getRow field (groupSquares grid))

-- use excluded values arrays of all related fields (use grid instead of [Field])
determineValueRow :: Field -> Int -> Row -> Maybe Field
determineValueRow _ _ [] = Nothing
-- Strategy 1 for solving Sudoku: if all possible values but 1 are excluded for a field (because related fields contain this value), this is the valid value
determineValueRow field y row | y > 0  && y <= gridSize && length(getExcludedValues field) == (gridSize -1 ) = if y `notElem` getExcludedValues field then Just (getLocation field, y, getExcludedValues field) else determineValueRow field (y + 1) row
-- Strategy 2 for solving Sudoku: if a value is excluded in all related fields (i.e. a square or row) than this must be the valid value for this field
determineValueRow field y row | y > 0  && y <= gridSize = if valueIsExcludedInRow y (removeValue field row) then Just (getLocation field, y, getExcludedValues field) else determineValueRow field (y + 1) row
determineValueRow _ _ _  = Nothing

valueIsExcludedInRow :: Int -> [Field] -> Bool 
valueIsExcludedInRow _ [] = False
valueIsExcludedInRow x xs =  all (elem x . getExcludedValues) xs

isValidValue :: Int -> [Field] -> Bool
isValidValue _ [] = False
isValidValue x xs = containsAllValues (removeValue x [1..gridSize]) xs

hasValue :: Int -> [Field] -> Bool
hasValue _ [] = False 
hasValue x fields = foldr ((||).(== x).getValue) False fields

containsAllValues :: [Int] -> [Field] -> Bool
containsAllValues _ [] = False
containsAllValues xs ys = all (\ z -> z `elem` map getValue ys) xs

removeValue :: (Eq a) => a -> [a] -> [a]
removeValue _ []              = []
removeValue x (y:ys) | x == y = removeValue x ys
                     | otherwise  = y : removeValue x ys

getLocation :: Field -> Location
getLocation (l,_,_) = l

getValue :: Field -> Value
getValue (_,v,_) = v

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
