import Util
import Data.List
import Data.Char (ord)
import Data.Maybe
import Debug.Trace

main = do
    contents <- readDay 9
    let nums = parseLine contents
        ordered = fsFile nums
        indexed = zipWith (\(x,y) z -> (x,y,z)) nums $ indices nums
        l = length $ reorderBlocks indexed
    print $ checksum ordered
    print $ checksum $ flattenBlocks $ reorderBlocks indexed

parseLine c = if even (length ns) then init ns else ns
    where ns = zipWith (\ x i -> (x, div i 2)) (map (\x -> ord x - ord '0') $ init c) [0..]

fsFile :: [(Int, Int)] -> [Int]
fsFile [] = []
fsFile ((f, i):rest) = replicate f i ++ fsEmpty rest

fsEmpty :: [(Int, Int)] -> [Int]
fsEmpty [] = []
fsEmpty [a] = []
fsEmpty [(e, _), (f, i)] = replicate f i
fsEmpty ((0, _):rest) = fsFile rest
fsEmpty ((e, _):rest) = replicate f i ++ fsEmpty ((e-f, 0):rest')
    where (end, i) = last rest
          f = min e end
          rest' = if end == f
                     then init (init rest)
                     else init rest ++ [(end - f, i)]

checksum xs = sum $ zipWith (*) xs [0..]

indices = foldl (\acc (n, _) -> acc ++ [n + last acc]) [0]

spaces xs = [(\(a,_,b) -> (a,0,b)) x | (x, i) <- zip xs [0 .. ], odd i]

files xs = [x | (x, i) <- zip xs [0..], even i]

removeAt i xs = (a, tail b)
    where (a, b) = splitAt i xs

cmpLast (_,_,z) (_,_,z') = compare z z'

fillSpace :: (Int, Int, Int) -> [(Int, Int, Int)] -> ((Int, Int, Int), [(Int, Int, Int)])
-- fillSpace f es | trace (show f ++ " " ++ show es) False = undefined
fillSpace f [] = (f, [])
fillSpace tri@(sz, id, ind) es@((s, _, i):rest)
  -- Nothing happens if we're past the file's current position
  | ind <= i = (tri, es)
  -- Put in a space for where the unit was pulled out so checksum is padded correctly
  -- Don't need to compact because the space is too deep for any future fills to use
  | sz == s = ((sz, id, i), sortBy cmpLast $ (sz, 0, ind) : rest)
  | sz < s = ((sz, id, i), (s-sz, 0, i+sz): sortBy cmpLast ((sz, 0, ind) : rest))
  | otherwise = (\(a, b) -> (a, (s, 0, i):b)) $ fillSpace tri rest

flattenBlocks :: [(Int, Int)] -> [Int]
flattenBlocks [] = []
flattenBlocks ((n, id):rest) = replicate n id ++ flattenBlocks rest

reorderBlocks :: [(Int, Int, Int)] -> [(Int, Int)]
reorderBlocks blocks = map (\(a,b,_) -> (a,b)) $ sortBy cmpLast compacted
    where compacted = uncurry (++) $ foldr (\f (arr, es) ->
            let (f', es') = fillSpace f es in (f':arr, es')) ([], space) file
          space = spaces blocks
          file = files blocks

