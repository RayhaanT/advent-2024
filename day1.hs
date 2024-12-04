import System.IO  
import Control.Monad
import Data.List

main = do  
        let list = []
        handle <- openFile "day1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        let odds = sort (oddInd list)
        let evens = sort (evenInd list)
        let diff = diffs odds evens
        -- Day 1
        print (mysum diff)
        -- Day 2
        print (mysum (prod odds evens))

        hClose handle

f :: [String] -> [Int]
f = map read

oddInd :: [Int] -> [Int]
oddInd [] = []
oddInd (x:y:xs) = x:oddInd xs

evenInd :: [Int] -> [Int]
evenInd [] = []
evenInd (x:y:xs) = y:evenInd xs

diffs :: [Int] -> [Int] -> [Int]
diffs [] [] = []
diffs (x:xs) (y:ys) = abs(x-y):diffs xs ys

mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

occurrences :: Int -> [Int] -> Int
occurrences x [] = 0
occurrences x (y:xs)=  if x == y then 1 + occurrences x xs else occurrences x xs

prod :: [Int] -> [Int] -> [Int]
prod [] l = []
prod (x:xs) l = x*(occurrences x l) : prod xs l
