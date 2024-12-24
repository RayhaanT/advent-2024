import Util
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

main = do
    contents <- readDay 12
    let grid = lines contents
        s = S.empty
    print $ bfsAll grid
    print $ allSides grid

bfsAll :: [String] -> Int
bfsAll g = fst $ foldl (\(n, s) p -> (\(a,b,c)->(n+length a*b,c)) $ bfs g s p) (0, S.empty) g'
    where g' = [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1]]

bfs :: [String] -> S.Set (Int, Int) -> (Int, Int) -> ([(Int, Int)], Int, S.Set (Int, Int))
bfs g s p@(x,y)
  | S.member p s = ([], 0, s)
  -- | trace (show p ++ show s) False = undefined
  | otherwise = addCurr $ foldl myFold ([],0,S.insert p s) vdeltas
  where c = (g!!y)!!x
        deltas = [add p dp | dp <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]
        vdeltas = filter (same g c) $ filter (valid g) deltas
        addCurr (a,b,s') = (p:a, b+(4 - length vdeltas), s')
        myFold (a,b,s') p' = (\(a',b',s'')->(a' ++ a,b+b',s'')) $ bfs g s' p'

valid g (x,y) = x >= 0 && y >= 0 && y < length g && x < length (head g)
same g c (x,y) = c == c' where c' = (g!!y)!!x
add (a,b) (c,d) = (a+c, b+d)

allSides :: [String] -> Int
allSides g = fst $ foldl (\(n, s) p -> (\(a,_,c)->(n+nsides a*length a,c)) $ bfs g s p) (0, S.empty) g'
    where g' = [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1]]

nsides :: [(Int, Int)] -> Int
nsides [] = 0
nsides [a] = 4
nsides ps = sum (map (corners ps) ps)

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
