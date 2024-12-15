import Util (readDay)
import Data.List

main = do
    contents <- readDay 4
    let rows = lines contents
        cols = transpose rows
        diag = diagonals rows
        antidiag = diagonals $ map reverse rows
    print (countXMAS rows + countXMAS cols + countXMAS diag + countXMAS antidiag)

    let tiles = getTiles rows
    print $ length $ filter verifyTile tiles

countXMAS :: [String] -> Int
countXMAS s = fcountSubstr "XMAS" s + rcountSubstr "XMAS" s

fcountSubstr  substr s = sum $ map (countSubstr substr) s
rcountSubstr substr s = fcountSubstr substr $ map reverse s

countSubstr substr s = length $ filter (isPrefixOf substr) (tails s)

diagonals :: [[a]] -> [[a]]
diagonals s = map reverse $ diags (replicate (length $ head s) []) s

diags :: [[a]] -> [[a]] -> [[a]]
diags tmp [] = reverse $ tail tmp
diags tmp grid = (last s : last tmp) : rec
    where s = head grid
          rec = diags ([] : zipWith (:) (init s) (init tmp)) (tail grid)

getTiles :: [[a]] -> [[[a]]]
getTiles grid
    | length grid < 3 = []
    | length (head grid) == 3 = take 3 grid : getTiles (tail grid)
    | otherwise = getTiles (transpose $ take 3 grid) ++ getTiles (tail grid)

countMAS s = fcountSubstr "MAS" s + rcountSubstr "MAS" s
verifyTile t = (countMAS d + countMAS dl) == 2
    where d = diagonals t
          dl = diagonals $ map reverse t