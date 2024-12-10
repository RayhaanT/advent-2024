import Util (readDay)
import Data.List

main = do
    contents <- readDay 1
    print $ part1 contents
    print $ part2 contents

columns c = transpose $ map (map read . words) $ lines c

part1 contents = do
    let [first, second] = map sort $ columns contents
    sum $ zipWith (\x y -> abs (x - y)) first second

part2 contents = do
    let [first, second] = columns contents
    sum (prod first second)

occurrences :: Eq a => a -> [a] -> Int
occurrences x xs = length $ filter (x ==) xs

prod :: [Int] -> [Int] -> [Int]
prod xs l = map (\ x -> x * occurrences x l) xs
