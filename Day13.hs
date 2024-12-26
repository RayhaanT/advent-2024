import Util
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Bifunctor

main = do
    contents <- readDay 13
    let ls = lines contents
        machines = parse ls
    print $ sum $ mapMaybe cost machines
    print $ sum $ mapMaybe cost2 machines

data Machine = Machine (Int, Int) (Int, Int) (Int, Int) deriving Show

read' :: String -> Int
read' s
  | head s == '+' = read $ tail s
  | otherwise = read s

parse :: [String] -> [Machine]
parse [] = []
parse ("":rest) = parse rest
parse (a:b:c:rest) = Machine a' b' c' : parse rest
    where atoX = drop 11 a
          btoX = drop 11 b
          a' = bimap read' (read' . drop 3) $ break (== ',') atoX
          b' = bimap read' (read' . drop 3) $ break (== ',') btoX
          ctoX = drop 9 c
          c' = bimap read (read . drop 4) $ break (== ',') ctoX

cost2 (Machine a b (cx, cy)) = cost (Machine a b (cx+10000000000000, cy+10000000000000))
cost :: Machine -> Maybe Int
cost (Machine a@(ax, ay) b@(bx, by) c@(cx, cy))
  | rhs `mod` q /= 0 = Nothing
  | n `mod` ax /= 0 = Nothing
  | otherwise = let a = n `div` ax in Just $ a*3 + b
  where rhs = cy*ax - cx*ay
        q = by*ax - bx*ay
        b = rhs `div` q
        n = cx - b*bx
