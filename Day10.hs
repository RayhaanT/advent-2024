import Util
import Data.List
import Data.Char

main = do
    contents <- readDay 10
    let grid = lines contents
        ends = map (paths grid) $ zeros grid
    print $ sum $ map (length . nub) ends
    print $ sum $ map length ends

add (a,b) (c,d) = (a+c, b+d)

zeros g = [(x, y) | (row, y) <- zip g [0..], (c, x) <- zip row [0..], c == '0']

paths :: [String] -> (Int, Int) -> [(Int, Int)]
paths g p@(x,y)
  | c == '9' = [p]
  | otherwise = concatMap (paths g) (filter (up1 g c) $ filter (valid g) deltas)
  where c = (g!!y)!!x
        deltas = [add p dp | dp <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]

valid g (x,y) = x >= 0 && y >= 0 && y < length g && x < length (head g)

up1 g c (x,y)
    | isDigit c' = digitToInt c' == digitToInt c + 1
    | otherwise = False
    where c' = (g!!y)!!x
