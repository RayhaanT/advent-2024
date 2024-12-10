module Util (readDay) where

import System.IO

readDay day = readFile ("inputs/day" ++ show day ++ ".txt")
