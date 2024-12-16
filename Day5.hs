import Util
import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

main = do
    contents <- readDay 5
    let rows = lines contents
        rules = multiMap $ map parseRule $ takeWhile (elem '|') rows
        updates = map parseUpdate $ tail $ dropWhile (elem '|') rows
    print $ sum $ map middle $ filter (verifyUpdate rules) updates
    let fixed = map (sortBy (\a b -> topoOrder a b rules)) $ filter (not . verifyUpdate rules) updates
    print $ sum $ map middle fixed

type Edges = M.Map Int (S.Set Int)

parseUpdate :: String -> [Int]
-- parseUpdate s = map read $ stringSplit (== ',') s
parseUpdate s = read ("[" ++ s ++ "]")

parseRule :: String -> (Int, Int)
parseRule r = mapPair read $ second tail $ break (== '|') r

multiMap :: [(Int, Int)] -> Edges
multiMap pairs = M.fromList $ map (\k -> (k, S.fromList $ map snd $ filter ((== k) . fst) pairs)) keys
    where keys = nub $ map fst pairs

verifyUpdate :: Edges -> [Int] -> Bool
verifyUpdate m ns = fst $ foldl
    (\(b, s) n ->
        (b && null (S.intersection (fromMaybe S.empty $ M.lookup n m) s), S.insert n s)
        )
    (True, S.empty) ns

middle [a] = a
middle [a, b, c] = b
middle (x:xs) = middle $ init xs

btord True = LT
btord False = GT

topoOrder :: Int -> Int -> Edges -> Ordering
topoOrder a b e
    | isNothing $ M.lookup a e = GT
    | S.member b $ fromJust $ M.lookup a e = LT
    | isNothing $ M.lookup b e = EQ
    | S.member a $ fromJust $ M.lookup b e = GT
    | otherwise = EQ
