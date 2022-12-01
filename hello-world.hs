import Data.List
import System.IO

toInt line = read line :: Int

mapLines2 :: [String] -> [Int] -> [[Int]]
mapLines2 [] list = [list]
mapLines2 ("" : lines) list = [list] ++ mapLines lines
mapLines2 (line : lines) list = mapLines2 lines (list ++ [toInt line])

mapLines :: [String] -> [[Int]]
mapLines lines = mapLines2 lines []


main = do
    content <- readFile "test-input.txt"
    let linesOfFile = lines content
    let mappedData = mapLines linesOfFile
    putStrLn (show mappedData)