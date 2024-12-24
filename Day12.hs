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
    -- print $ bfs grid S.empty (0,1)

uFind :: M.Map Int Int -> Int -> (Int, M.Map Int Int)
uFind m n
    | Just p <- parent = uFind (maybe m (\x -> M.insert n x m) (M.lookup p m)) p
    | otherwise = (n, m)
    where parent = M.lookup n m

uUnion :: M.Map Int Int -> Int -> Int -> M.Map Int Int
uUnion m a b = M.insert a b m

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

data Dir = U | D | L | R deriving (Eq, Show)
right U = R
right R = D
right D = L
right L = U
left U = L
left L = D
left D = R
left R = U
delta U = (0,-1)
delta D = (0,1)
delta R = (1,0)
delta L = (-1,0)

add' (a,b,e) (c,d,e') = (a+c,b+d,e')
sub (a,b) (c,d) = (a-c, b-d)

allSides :: [String] -> Int
allSides g = fst $ foldl (\(n, s) p -> (\(a,_,c)->(n+nsides a*length a,c)) $ bfs g s p) (0, S.empty) g'
    where g' = [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1]]

extract (a,b,c) = (a,c)

nsides :: [(Int, Int)] -> Int
nsides s = fst $ foldl
    (\(n, set) p -> if p `S.member` set then (n,set) else let (n',set') = walkFrom s p in (n'+n, S.union set set'))
    (0, S.empty) interest
    where interest = [ p | p <- s, add p (delta U) `notElem` s ]

eq (a,b,_) (a',b',_) = (a == a') && (b == b')

walkFrom :: [(Int, Int)] -> (Int, Int) -> (Int, S.Set (Int, Int))
walkFrom a b | trace (show b) False = undefined
walkFrom [] _ = (0, S.empty)
walkFrom s start = extract $ until (\(i,p,_) -> p `eq` start' && i > 0) (\(i,p,seen) -> stepCount s seen i p) (0, start', S.empty)
    where start' = uncurry (,,R) start
    -- (\(x,y)->(x,y,R)) $ until (`notElem` s) (\(x,y) -> (x,y-1)) $ head s

stepCount [a] s _ t@(x,y,d) = (4, t, S.insert (x,y) s)
stepCount region seen i t@(x,y,d)
  -- | trace (show t ++ " " ++ show i) False = undefined
  | d' == d = (i,t',seen')
  | otherwise = (i+1,t',seen')
  where t'@(_,_,d') = step region t
        seen' = S.insert (x,y) seen

step :: [(Int, Int)] -> (Int, Int, Dir) -> (Int, Int, Dir)
step s t@(x,y,d)
  | (x, y) `notElem` s = add' t $ uncurry ( , , right d) $ add (sub (0,0) $ delta d) $ delta $ right d
  | rail `elem` s = add' t $ uncurry ( , , left d) $ delta $ left d
  | otherwise = add' t $ uncurry ( , , d) $ delta d
  where rail = add (x,y) $ delta $ left d
