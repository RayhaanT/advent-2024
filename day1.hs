module Day1 (part1, part2) where

import Data.List
import System.IO

main = withFile "day1.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    print $ part1 contents
    print $ part2 contents

part1 contents = do
    let list = map read $ words contents
        odds = sort (oddInd list)
        evens = sort (evenInd list)
        diff = diffs evens odds

    sum diff

part2 contents = do
    let list = map read $ words contents
        odds = sort (oddInd list)
        evens = sort (evenInd list)

    sum (prod odds evens)

f :: [String] -> [Int]
f = map read

oddInd xs = [ x | (x, i) <- zip xs [1..length xs], odd i ]
evenInd xs = [ x | (x, i) <- zip xs [1..length xs], even i ]
diffs = zipWith (\ x y -> abs (x - y))

occurrences :: Eq a => a -> [a] -> Int
occurrences x xs = length $ filter (x ==) xs

prod :: [Int] -> [Int] -> [Int]
prod xs l = map (\ x -> x * occurrences x l) xs
