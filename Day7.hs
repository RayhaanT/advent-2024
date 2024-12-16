import Util
import Data.List
import Data.Maybe
import qualified Data.Map as M

main = do
    contents <- readDay 7
    let rows :: [(Int, [Int])] = map (
                (\(n, arr) -> (read n, map read $ words $ tail $ tail arr)) .
                (\r -> splitAt (fromJust $ elemIndex ':' r) r)
                ) (lines contents)
    print $ sum $ map fst $ filter (\(t, arr) -> solvable arr t) rows
    print $ sum $ map fst $ filter (\(t, arr) -> solvable2 arr t) rows

solvable :: [Int] -> Int -> Bool
solvable [a] t = a == t
solvable ns t = solvable (init ns) (t - x) || (t `mod` x == 0 && solvable (init ns) (div t x))
    where x = last ns

solvable2 :: [Int] -> Int -> Bool
solvable2 [a] t = a == t
solvable2 ns t = solvable2 (init ns) (t - x)
                || (t `mod` x == 0 && solvable2 (init ns) (div t x))
                || maybe False (solvable2 (init ns)) (unconcat t x)
    where x = last ns

iconcat :: Int -> Int -> Int
iconcat a b = read $ show a ++ show b

unconcat :: Int -> Int -> Maybe Int
unconcat t x
    | abs t == abs x = Just 0
    | sx `isSuffixOf` st = Just $ read $ reverse $ drop (length sx) $ reverse st 
    | otherwise = Nothing
    where sx = show x
          st = show t
