import Util
import Data.List
import qualified Data.Map as M

main = do
    contents <- readDay 11
    let stones :: [Int] = map read $ words contents
        sm = M.fromList $ zip stones [1,1..]
    print stones
    print $ sum $ map snd $ M.toList $ iterate evolveAll sm!!25
    print $ sum $ map snd $ M.toList $ iterate evolveAll sm!!75

evolveAll :: M.Map Int Int -> M.Map Int Int
evolveAll m = compact M.empty $ concatMap (\(s, n) -> zip (evolve s) [n,n..]) (M.toList m)

compact :: M.Map Int Int -> [(Int, Int)] -> M.Map Int Int
compact m [] = m
compact m ((s,n):rest)
  | Just n' <- e = compact (M.insert s (n + n') m) rest
  | Nothing <- e = compact (M.insert s n m) rest
  where e = M.lookup s m

evolve 0 = [1]
evolve n
  | even (length sn) = map read $ (\(x,y) -> [x,y]) $ splitAt (length sn `div` 2) sn
  | otherwise = [n*2024]
  where sn = show n
