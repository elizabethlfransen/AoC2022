import Data.List
import System.IO

toInt line = read line :: Int

mapLines2 :: [String] -> [Int] -> [[Int]]
mapLines2 [] list = [list]
mapLines2 ("" : lines) list = [list] ++ mapLines lines
mapLines2 (line : lines) list = mapLines2 lines (list ++ [toInt line])

mapLines :: [String] -> [[Int]]
mapLines lines = mapLines2 lines []

processInput :: [String] -> Int
processInput input = 
    foldl max 0 totalCalorieList
    where 
        calorieList = mapLines input
        totalCalorieList = map mapFunc calorieList
        mapFunc :: [Int] -> Int
        mapFunc x = foldl (+) 0 x

main = do
    content <- readFile "day1.input.txt"
    let linesOfFile = lines content
    putStrLn (show (processInput linesOfFile))