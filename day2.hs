import System.IO
import Control.Monad
import Data.List

main = do
        -- Input
        let list = []
        handle <- openFile "day2.txt" ReadMode
        contents <- hGetContents handle
        let rows = map (f . words) (lines contents)

        -- Part 1
        print (sum (map (b2i . safe) rows))
        -- Part 2
        let x = map variants rows
        print (sum (map ((b2i . or) . map safe) x))

        hClose handle

f :: [String] -> [Int]
f = map read

increasing :: [Int] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = if x >= y then False else increasing (y:xs)

decreasing :: [Int] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) = if x <= y then False else decreasing (y:xs)

close :: [Int] -> Bool
close [] = True
close [x] = True
close (x:y:xs) = if abs (x-y) <= 3 && abs (x-y) >= 1
                    then close (y:xs)
                    else False

logic :: Bool -> Bool -> Bool -> Bool
logic x y z = (x && z) || (y && z)

safe :: [Int] -> Bool
safe x = do
    let i = increasing x
    let d = decreasing x
    let c = close x
    logic i d c

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

variants :: [Int] -> [[Int]]
variants [] = []
variants (x:xs) = xs : map (x :) (variants xs)
