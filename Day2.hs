import Util (readDay)
import Data.List

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

increasing :: [Int] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = y > x && increasing (y:xs)

decreasing :: [Int] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) = y < x && decreasing (y:xs)

close :: [Int] -> Bool
close [] = True
close [x] = True
close (x:y:xs) = (abs (x-y) <= 3 && abs (x-y) >= 1) && close (y:xs)

logic :: Bool -> Bool -> Bool -> Bool
logic x y z = (x && z) || (y && z)

safe :: [Int] -> Bool
safe x = logic (increasing x) (decreasing x) (close x)

variants :: [Int] -> [[Int]]
variants [] = []
variants (x:xs) = xs : map (x :) (variants xs)
