import Util (readDay)
import Data.List
import GHC.Base

main = do
    contents <- readDay 2
    print $ part1 contents
    print $ part2 contents

part1 contents = do
    let rows = map (map read . words) (lines contents)
    sum (map (fromEnum . safe) rows)

part2 contents = do
    let rows = map (map read . words) (lines contents)
        x = map variants rows
    sum (map ((fromEnum . or) . map safe) x)

increasing xs = fst $ foldl (\(acc, y) x -> (acc && x > y, x)) (True, minInt) xs
decreasing xs = fst $ foldl (\(acc, y) x -> (acc && x < y, x)) (True, maxInt) xs
close xs = fst $ foldl
    (\(acc, y) x -> let d = abs(x - y) in (acc && d <= 3 && d >= 1, x))
    (True, head xs + 1) xs

safe x = close x && (increasing x || decreasing x)

variants :: [Int] -> [[Int]]
variants [] = []
variants (x:xs) = xs : map (x :) (variants xs)
