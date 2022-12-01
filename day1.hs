import Data.List
import System.IO

toInt line = read line :: Int

mapLines2 :: [String] -> [Int] -> [[Int]]
mapLines2 [] list = [list]
mapLines2 ("" : lines) list = [list] ++ mapLines lines
mapLines2 (line : lines) list = mapLines2 lines (list ++ [toInt line])

mapLines :: [String] -> [[Int]]
mapLines lines = mapLines2 lines []

max3 :: (Int, Int, Int) -> Int -> (Int, Int, Int)
max3 (top1, top2, top3) new
    | new > top1 = (new, top1, top2)
    | new > top2 = (top1, new, top2)
    | new > top3 = (top1, top2, new)
max3 (top1, top2, top3) new = (top1, top2, top3)

processInput :: [String] -> Int
processInput input = 
    mc1 + mc2 + mc3
    where
        (mc1, mc2, mc3) = foldl max3 (0,0,0) totalCalorieList 
        calorieList = mapLines input
        totalCalorieList = map mapFunc calorieList
        mapFunc :: [Int] -> Int
        mapFunc x = foldl (+) 0 x

main = do
    content <- readFile "day1.input.txt"
    let linesOfFile = lines content
    putStrLn (show (processInput linesOfFile))