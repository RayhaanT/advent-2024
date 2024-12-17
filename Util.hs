module Util (readDay, mapPair, stringSplit, printGrid, printList) where

import System.IO
import Data.Bifunctor

readDay day = readFile ("inputs/day" ++ show day ++ ".txt")

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = bimap f f

stringSplit :: (Char -> Bool) -> String -> [String]
stringSplit p s = case dropWhile p s of
                    "" -> []
                    s' -> w : stringSplit p rest
                        where (w, rest) = break p s'

printGrid :: [String] -> IO ()
printGrid = mapM_ putStrLn

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print
