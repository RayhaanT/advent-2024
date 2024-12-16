module Util (readDay, mapPair, stringSplit) where

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
