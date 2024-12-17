import Util
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    contents <- readDay 8
    let grid = lines contents
        freqs = nub [ c | row <- grid, c <- row, c /= '.' ]
        ps = map (antennae grid) freqs
        antis = nub $ concatMap (filter (valid grid) . nub . antinodes) ps
        antis2 = nub $ concatMap (filter (valid grid) . nub . antinodes2 (length $ head grid) (length grid)) ps
    drawNodes grid antis
    putStrLn ""
    drawNodes grid antis2
    print $ length antis2

type Pos = (Int, Int)

antennae :: [String] -> Char -> [Pos]
antennae g f = [ (x, y) | (row, y) <- zip g [0..], (c, x) <- zip row [0..], c == f ]

antinodes :: [Pos] -> [Pos]
antinodes ps = [ p | a <- ps, b <- ps, a /= b, p <- antinodePair a b ]

antinodePair (x,y) (x',y') = [(x-dx, y-dy), (x'+dx, y'+dy)]
                        where dx = x' - x
                              dy = y' - y

mapAntinodes2 grid ps = nub $ concatMap (filter (valid grid) . nub . antinodes2 (length $ head grid) (length grid)) ps
antinodes2 w h ps = [ p | a <- ps, b <- ps, a /= b, p <- antinodeLine w h a b ]

generateUntil valid f i = tail $ until (not . valid . head) (\ps -> f (head ps) : ps) [i]
plus (x,y) (x',y') = (x+x', y+y')
minus (x,y) (x',y') = (x-x', y-y')

lineInterval :: (Pos -> Bool) -> Pos -> Pos -> [Pos]
lineInterval valid i d = generateUntil valid (plus d) i ++ generateUntil valid (`minus` d) i

antinodeLine :: Int -> Int -> Pos -> Pos -> [Pos]
antinodeLine w h a@(x, y) (x', y') = lineInterval (valid' w h) a d
  where d = reduceDelta dx dy
        dx = x' - x
        dy = y' - y

valid g = valid' (length (head g)) (length g)
valid' w h (x, y) = x >= 0 && y >= 0 && x < w && y < h

drawNodes g as = printGrid [ [ if elem (x, y) as then '#' else c | (c, x) <- zip row [0..] ] | (row, y) <- zip g [0..] ]

gcd' a b
    | r == 0 = b
    | otherwise = gcd' b r
    where r = a `mod` b

reduceDelta x y = (div x m, div y m) where m = gcd' x y
