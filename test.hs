--program made to test various functions as they are being written to
--fix bugs in a controlled environment
--Alexander Minser


import Data.List.Split
import System.IO
import Debug.Trace

--read numbers from .csv file
readLines :: [[String]] -> [Int]
readLines fileLines =
    concatMap (map read) fileLines

--creates indexes for board rows
getRowIndexes :: Int -> [Int]
getRowIndexes indexVal =
    let leftMostIndex = last $ takeWhile (<=indexVal) [0, 9..81]
    in map (+leftMostIndex) [0..8]

--creats indexes for board columns
getColumnIndexes :: Int -> [Int]
getColumnIndexes indexVal =
    let topMostIndex = indexVal - (head $ dropWhile (\x -> indexVal - x > 8) [0, 9..81])
    in map (+topMostIndex) [0, 9..72]

--main, opens file and tests functions
--last function to be tested: getRowIndexes
main :: IO()
main = do
    handle <- openFile "testPuzzle.csv" ReadMode
    contents <- hGetContents handle
    let linesSplit = map (splitOn ",") (lines contents)
    let linesRead = readLines linesSplit
    let rowIndexes = concatMap getRowIndexes linesRead
    print $ 81 `elem` rowIndexes
