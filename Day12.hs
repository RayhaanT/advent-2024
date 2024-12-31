import Util
import Data.List
import qualified Data.Set as S
import Debug.Trace
import Control.Monad
import Control.Monad.State

main = do
    contents <- readDay 12
    let grid = lines contents
        s = S.empty
    print $ part1 grid
    print $ part2 grid

part1 g = foldl (\acc (ps, perim) -> acc + perim*length ps) 0 $ segment g

segment :: [String] -> [([(Int, Int)], Int)]
segment g = filter (not . null . fst) $ evalState (mapM helper g') S.empty
    where g' = [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1]]
          helper p = do
              s <- get
              if S.member p s
                 then return ([], 0)
                 else do
                     let (r,perim) = bfs g p
                     put (S.union r s)
                     return (S.toList r, perim)

bfs :: [String] -> (Int, Int) -> (S.Set (Int, Int), Int)
bfs g p = execState (bfs' g p) (S.empty, 0)

bfs' :: [String] -> (Int, Int) -> State (S.Set (Int, Int), Int) ()
bfs' g p@(x,y) = do
    (s, perim) <- get
    unless (S.member p s) $
        put (S.insert p s, perim + 4 - length vdeltas) >> mapM_ (bfs' g) vdeltas
        where c = (g!!y)!!x
              deltas = [add p dp | dp <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]
              vdeltas = filter (same g c) $ filter (valid g) deltas

part2 g = foldl (\acc (ps, perim) -> acc + nsides ps*length ps) 0 $ segment g

valid g (x,y) = x >= 0 && y >= 0 && y < length g && x < length (head g)
same g c (x,y) = c == c' where c' = (g!!y)!!x
add (a,b) (c,d) = (a+c, b+d)

nsides [] = 0
nsides [a] = 4
nsides ps = sum $ map (corners ps) ps

corners :: [(Int, Int)] -> (Int, Int) -> Int
corners ps p = interiorCorners ps p + case length vdeltas + length hdeltas of
                    1 -> 2
                    2 -> if length vdeltas == 1 then 1 else 0
                    _ -> 0
    where vdeltas = filter (`elem` ps) [add p dp | dp <- [(0,1), (0,-1)]]
          hdeltas = filter (`elem` ps) [add p dp | dp <- [(1,0), (-1,0)]]

helper d a b c = fromEnum (d!!a && d!!b && not (d!!c))

interiorCorners :: [(Int, Int)] -> (Int, Int) -> Int
interiorCorners ps p = helper mdeltas 0 2 1 + helper mdeltas 2 4 3 +
        helper mdeltas 4 6 5 + helper mdeltas 6 0 7
    -- 701
    -- 6.2
    -- 543
    where deltas = [add p dp | dp <- [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]]
          mdeltas = map (`elem` ps) deltas
