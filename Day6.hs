import Util
import Data.List
import Data.Maybe
import Data.Bifunctor
import qualified Data.Set as S
import Debug.Trace

main = do
    contents <- readDay 6
    let grid = lines contents
        g = findGuard grid
        visits = S.empty
        (grid', g', visits') = until (\(_, g'', _) -> invalid grid g'') moveGuardLp (grid, g, visits)
    printPath visits'
    print $ length visits'
    let variants = [ [ [ if gx == x && gy == y then '#' else c | (c, gx) <- zip row [0..] ]
                        | (row, gy) <- zip grid [0..] ] | (x, y) <- S.toList visits' ]
    print $ length $ filter (not . terminates g) variants

data Direction = Up | Rght | Down | Lft deriving (Show, Eq, Ord)
type Guard = (Int, Int, Direction)

dirSucc Up = Rght
dirSucc Rght = Down
dirSucc Down = Lft
dirSucc Lft = Up

invalid :: [String] -> Guard -> Bool
invalid grid (x, y, d) = x < 0 || y < 0 || x >= length (head grid) || y >= length grid

findGuard :: [String] -> Guard
findGuard [a] = (fromJust $ elemIndex '^' a, 0, Up)
findGuard (r:xs) = maybe (x, y + 1, Up) (, 0, Up) (elemIndex '^' r)
    where (x, y, d) = findGuard xs

moveGuardLp (grid, g, visits) = (grid, g', updVisits (invalid grid) visits g g')
    where g' = moveGuard grid g

moveGuard :: [String] -> Guard -> Guard
moveGuard grid guard@(x,y,d) = case d of
    Down -> (x, , d') $ maybe (length grid) (\y' -> y' - 1 + length precol) $ elemIndex '#' postcol
    Up -> (x, , d') $ maybe (-1) (\y' -> length precol - y') $ elemIndex '#' $ reverse precol
    Rght -> (, y, d') $ maybe (length (head grid)) (\x' -> x' - 1 + length prerow) $ elemIndex '#' postrow
    Lft -> (, y, d') $ maybe (-1) (\x' -> length prerow - x') $ elemIndex '#' $ reverse prerow
    where col = transpose grid !! x
          row = grid !! y
          (precol, postcol) = splitAt y col
          (prerow, postrow) = splitAt x row
          d' = dirSucc d

terminateInner :: S.Set Guard -> Guard -> [String] -> Bool
terminateInner hist guard grid
    | S.member guard hist = False
    | invalid grid guard = True
    | otherwise = terminateInner (S.insert guard hist) (moveGuard grid guard) grid
terminates = terminateInner S.empty

interval a b = if a <= b then [a..b] else [a,a-1..b]

updVisits :: (Guard -> Bool) -> S.Set (Int, Int) -> Guard -> Guard -> S.Set (Int, Int)
updVisits invalidPos s g@(x,y,d) g'@(x',y',_)
    | invalidPos g = s
    | x == x' && y == y' = s
    | otherwise = foldr S.insert s [ (dx, dy) | dx <- interval x x', dy <- interval y y', not $ invalidPos (dx, dy, Up) ]

printPath s = mapM_ putStrLn $ showPath s
showPath :: S.Set (Int, Int) -> [String]
showPath s = [ [ (if S.member (x, y) s then '#' else '.') | x <- [0..w] ] | y <- [0..h] ]
    where w = 1 + maximum (map fst $ S.toList s)
          h = 1 + maximum (map snd $ S.toList s)
