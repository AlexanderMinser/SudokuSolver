
import System.IO
import Data.List as List
import Data.List.Split
import Data.Set as Set (toList, fromList)
import Data.Maybe as Maybe

--data type for holding information about each individual board 'square'
data Square = Square {
    solved :: Bool,
    solution :: Int,
    possibleSolutions :: [Int],
    index :: Int,
    relatedIndexes :: [Int]
}

--type alias for list of squares representing the the game 'board'
type Board = [Square]


--getRowIndexes, getColumnIndexes, and getBoxIndexes are used exclusively for buildSquare
--helper functions
getRowIndexes :: Int -> [Int]
getRowIndexes indexVal =
    let leftMostIndex = last $ takeWhile (<=indexVal) [0, 9..81]
    in map (+leftMostIndex) [0..8]

getColumnIndexes :: Int -> [Int]
getColumnIndexes indexVal =
    let topMostIndex = indexVal - (head $ dropWhile (\x -> indexVal - x > 8) [0, 9..81])
    in map (+topMostIndex) [0, 9..72]

getBoxIndexes :: Int -> [Int] 
getBoxIndexes indexVal =
    let indexGroup1 = [0,1,2,9,10,11,18,19,20]
        indexGroups = [  indexGroup1           ,
                         map (+3) indexGroup1  ,
                         map (+6) indexGroup1  ,
                         map (+27) indexGroup1 ,
                         map (+30) indexGroup1 ,
                         map (+33) indexGroup1 ,
                         map (+54) indexGroup1 ,
                         map (+57) indexGroup1 ,
                         map (+60) indexGroup1  ]
    in Maybe.fromMaybe [] $ find (indexVal `elem`) indexGroups



buildSquare :: Int -> Int -> Square
buildSquare indexVal num =
    let solvedVal = num /= 0
        possibleSolutionsVal = if num == 0 then [1..9] else [num]
        relatedIndexesVal = getRelatedIndexes indexVal
    in Square solvedVal num possibleSolutionsVal indexVal relatedIndexesVal
    where
        getRelatedIndexes indexVal =
            let rowIndexes = getRowIndexes indexVal
                columnIndexes = getColumnIndexes indexVal
                boxIndexes = getBoxIndexes indexVal
            in removeDuplicates $ rowIndexes ++ columnIndexes ++ boxIndexes
            where
                removeDuplicates = Set.toList . Set.fromList

buildBoard :: String -> Board
buildBoard fileLines =
    let linesSplit = map (splitOn ",") (lines fileLines)
        linesReadAndFlattened = concatMap (map read) linesSplit
        in buildSquares 0 linesReadAndFlattened
        where
            buildSquares 81 allNums = []
            buildSquares currSquareIndex allNums =
                (buildSquare currSquareIndex currSquareNum) : (buildSquares (currSquareIndex+1) allNums)
                where
                    currSquareNum = allNums !! currSquareIndex

getRelatedSquares :: [Int] -> Board -> [Square]
getRelatedSquares relatedIndexesVal board =
    map (\indexVal -> board !! indexVal) relatedIndexesVal

--checks to see if square only has one possible value left, and if true then
--sets the 'solved' field to true
updateSolved :: [Int] -> Square -> Square
updateSolved updatedPossibleSolutions@(x:xs) (Square solvedVal solutionVal _ indexVal relatedIndexesVal)
    | null xs = Square True x [x] indexVal relatedIndexesVal
    | otherwise = Square solvedVal solutionVal updatedPossibleSolutions indexVal relatedIndexesVal


--removes already solved values in adjacent squares from possible solutions list
updatePossibleSolutions :: [Int] -> [Int] -> [Int]
updatePossibleSolutions oldSolutions [x]
    | x `elem` oldSolutions = delete x oldSolutions
    | otherwise = oldSolutions
updatePossibleSolutions oldSolutions comparedSolutions =
    oldSolutions

--looks at adjacent squares to see if they are solved for a value that is still
--a possible solution from square passed to function, then removes that value
--from possible solutions
updateSquare :: Square -> Board -> Square
updateSquare square@(Square solved _ possibleSolutionsIn index relatedIndexes) board =
    if not solved then
        let adjacentPossibleSolutions = map possibleSolutions $ getRelatedSquares relatedIndexes board
            updatedPossibleSolutions = foldr (flip updatePossibleSolutions) possibleSolutionsIn adjacentPossibleSolutions
        in updateSolved updatedPossibleSolutions square
    else
        square

--recursively updates squares until all squares are solved, then returns board
solvePuzzle :: Board -> Board
solvePuzzle board =
    if not $ puzzleSolved board then
        let boardUpdated = map (`updateSquare` board) board
        in solvePuzzle boardUpdated
    else
        board
    where
        puzzleSolved = all solved

showSolution :: Board -> IO()
showSolution board = helper board
    where
        helper [] = return ()
        helper board =
            (mapM_ (putStr . show . solution) $ take 9 board)
            >> (putStrLn "") >> (helper $ drop 9 board)


--main function
--opens data file with test 'board', creates representation of board in
--program, and solves the puzzle
main :: IO()
main = do
    handle <- openFile "testPuzzle.csv" ReadMode
    contents <- hGetContents handle
    let board = buildBoard contents
    let solution = solvePuzzle board
    showSolution solution
    hClose handle
