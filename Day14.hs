import Util
import Data.List
import Debug.Trace
import Data.Bifunctor
import Control.Monad

w = 101
h = 103
start = 6000

main = do
    contents <- readDay 14
    let rows = lines contents
        robots = map parseLine rows
        evolved = map (iterate updateRobot) robots
    print $ safety $ map (getP . (!!100)) evolved
    let init = map (!!start) evolved
        evolved' = map (iterate updateRobot) init
    forM_ [0..] $ \i -> do
        let r = map (getP . (!!i)) evolved'
        when (i `mod` 1000 == 0) $ print i
        when (treePattern r) $ do
            printRobots r
            putStrLn $ "Iteration: " ++ show (i + start)
            _ <- getLine
            return ()

data Robot = Robot (Int, Int) (Int, Int) deriving Show
parseLine s = uncurry Robot $ bimap parseTuple (parseTuple . drop 1) $ break (== ' ') s
parseTuple :: String -> (Int, Int)
parseTuple s = bimap read (read . drop 1) $ break (== ',') $ drop 2 s

getP (Robot p _) = p

updateRobot (Robot (px, py) v@(vx, vy)) = Robot ((px + vx) `mod` w, (py + vy) `mod` h) v

add (a,b,c,d) (e,f,g,h) = (a+e,b+f,c+g,d+h)
safety ps = a*b*c*d where (a,b,c,d) = quadrants ps
quadrants :: [(Int, Int)] -> (Int, Int, Int, Int)
quadrants [] = (0, 0, 0, 0)
quadrants ((x, y):ps) =
    add (quadrants ps) $ selectQuadrant x y
  where
    selectQuadrant x y
      | x < wh && y < hh = (1, 0, 0, 0)
      | x > wh && y < hh = (0, 1, 0, 0)
      | x < wh && y > hh = (0, 0, 1, 0)
      | x > wh && y > hh = (0, 0, 0, 1)
      | otherwise                          = (0, 0, 0, 0)
      where (wh, hh) = (w `div` 2, h `div` 2)

robotGrid r = [[if (x,y) `elem` r then '#' else '.' | x <- [0..w-1] ] | y <- [0..h-1]]
printRobots r = printGrid $ robotGrid r

nextn n (x,y) = [(x + i, y) | i <- [1..n]]
treePattern r = or [ all (`elem` r) $ nextn 9 p | p <- r ]
