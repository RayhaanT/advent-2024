import Util
import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    contents <- readDay 5
    let rows = lines contents
        rules = multiMap $ map parseRule $ takeWhile (elem '|') rows
        updates = map parseUpdate $ tail $ dropWhile (elem '|') rows
    print $ sum $ map middle $ filter (verifyUpdate rules) updates

parseUpdate :: String -> [Int]
-- parseUpdate s = map read $ stringSplit (== ',') s
parseUpdate s = read ("[" ++ s ++ "]")

parseRule :: String -> (Int, Int)
parseRule r = mapPair read $ second tail $ break (== '|') r

multiMap :: [(Int, Int)] -> M.Map Int (S.Set Int)
multiMap pairs = M.fromList $ map (\k -> (k, S.fromList $ map snd $ filter ((== k) . fst) pairs)) keys
    where keys = nub $ map fst pairs

verifyUpdate :: M.Map Int (S.Set Int) -> [Int] -> Bool
verifyUpdate m ns = fst $ foldl
    (\acc n ->
        (fst acc && null (S.intersection (maybe S.empty id $ M.lookup n m) (snd acc)), S.insert n $ snd acc)
        )
    (True, S.empty) ns

middle [a] = a
middle [a, b, c] = b
middle (x:xs) = middle $ init xs
