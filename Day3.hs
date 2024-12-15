import Util (readDay)
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

main = do
    contents <- readDay 3
    print $ parseLine False contents
    print $ parseLine True contents

parseLine b s = foldl (\acc (x, y) -> acc + x*y) 0 $ match b s

findSublistIndex :: Eq a => [a] -> [a] -> Maybe Int
findSublistIndex sublist list = findIndex (isPrefixOf sublist) (tails list)

match :: Bool -> String -> [(Int, Int)]
match _ [] = []
-- match s@(f:rest) | trace (show s) False = undefined
match part2 s@(f:rest)
    | part2 && "don't()" `isPrefixOf` s = case findSublistIndex "do()" rest of
        Just ind -> match part2 $ drop ind s
        Nothing -> []
    | "mul(" `isPrefixOf` s = case parseMul $ drop 4 s of
        Just p -> p:match part2 rest
        Nothing -> match part2 rest
    | otherwise = match part2 rest

parseMul :: String -> Maybe (Int, Int)
parseMul s
    | Just _ <- pind = parseBody $ takeWhile (/= ')') s
    | otherwise = Nothing
    where pind = elemIndex ')' s

mapTuple f (a, b) = (f a, f b)

split i s = case splitAt i s of
    (a, b) -> (a, tail b)

parseBody :: String -> Maybe (Int, Int)
parseBody s
    | Just ind <- cind = let sp = split ind s in
        if isNumeral (fst sp) && isNumeral (snd sp)
            then Just (mapTuple read sp)
            else Nothing
    | otherwise = Nothing
    where cind = elemIndex ',' s

isNumeral s = not (null s) && all isDigit s
