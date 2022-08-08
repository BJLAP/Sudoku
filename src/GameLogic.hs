
module GameLogic(solve,inputGrid,createGrid) where

type Value = Int
type HorPos = Int
type VerPos = Int
type Location = (HorPos, VerPos)
type Row = [Field]
type InputValues = [[Value]]
type Grid = [[Field]]
type Field = (Location, Value)

input = [[1,0,3,0], [2,0,0,0],[0,0,0,0],[4,0,1,0]] :: InputValues
inputGrid = createGrid input 0 

solve :: Grid -> Grid
solve grid | isComplete grid = grid
solve grid = solve (updateGrid grid (findNextValues grid))

test = concat (updateGrid (createGrid input 0)  [Just ((0,0), 3)])

createGrid :: InputValues -> Int -> Grid
createGrid [] _ = []
createGrid (x:xs) init | init < 4 = createTuples x init 0 : createGrid xs (init + 1)
createGrid _ _ = []

createTuples :: [Value] -> HorPos -> VerPos -> [Field]
createTuples [] _ _ = []
createTuples (x:xs) xpos ypos | ypos < 4 = ((xpos, ypos), x) : createTuples xs xpos (ypos + 1)
createTuples _ _ _ = []

updateGrid :: Grid -> [Maybe Field] -> Grid
updateGrid grid fields = [updateRow row fields | row <- grid]

updateRow :: Row -> [Maybe Field] -> Row
updateRow row fields  = [ updateField fields fieldToUpdate | fieldToUpdate <- row]

updateField :: [Maybe Field] -> Field -> Field
updateField [] field = field
updateField (Just x:xs) field = if fst x == fst field then x else updateField xs field
updateField (Nothing:xs) field = updateField xs field


isComplete :: Grid -> Bool
isComplete grid = foldr ((&&).(>0).snd) True (concat grid)

findNextValues :: Grid -> [Maybe Field]
findNextValues grid = map (findValue grid) (concat grid)  -- use map with partial function

findValue :: Grid -> Field -> Maybe Field
findValue grid field | snd field == 0 = determineValue field (findRelatedFields field grid)
findValue _ _ = Nothing

groupVertical :: Grid -> Grid
groupVertical [[],[],[],[]] = []
groupVertical (a:b:c:d:ds) = [head a, head b, head c, head d] : groupVertical [tail a,tail b,tail c,tail d]

groupSquares :: [[a]] -> [[a]]
groupSquares [[]] = []
--groupSquares grid = concat (map (splitList 4) (map (combineNth 4) (splitList 8 (concat grid))))
groupSquares grid = concatMap (splitList 4 . combineNth 4) (splitList 8 (concat grid))

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n list | n < 1 || n > length list = []
splitList n list = take n list : splitList n (reverse(take (length list-n) (reverse list))) 

combineNth :: Int -> [a] -> [a]
combineNth  _ [] = []
combineNth n list | length list <= n  = []
combineNth n (x:xs) = x : xs!!(n-1) : combineNth n xs

findRelatedFields :: Field -> Grid -> [Field]
findRelatedFields field grid = concatMap (listContainsField field) grid ++ concatMap (listContainsField field) (groupVertical grid) ++  concatMap (listContainsField field) (groupSquares grid)

listContainsField :: Field -> [Field] -> [Field]
listContainsField field fields = if foldr ((||).(== fst field).fst)False fields then fields else []

determineValue :: Field -> [Field] -> Maybe Field
determineValue _ [] = Nothing
determineValue field fields | hasValue 1 fields &&  hasValue 2 fields &&  hasValue 3 fields  = Just (fst field, 4)
determineValue field fields | hasValue 1 fields &&  hasValue 2 fields &&  hasValue 4 fields  = Just (fst field, 3)
determineValue field fields | hasValue 1 fields &&  hasValue 3 fields &&  hasValue 4 fields  = Just (fst field, 2)
determineValue field fields | hasValue 2 fields &&  hasValue 3 fields &&  hasValue 4 fields  = Just (fst field, 1)
determineValue field fields = Nothing

hasValue :: Int -> [Field] -> Bool
hasValue _ [] = False 
hasValue x fields = foldr ((||).(== x).snd) False fields
