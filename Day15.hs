import Util
import Data.List
import Data.Bifunctor
import Debug.Trace

main = do
    contents <- readDay 15
    let grid = parseGrid $ takeWhile (not . null) $ lines contents
        moves = concat $ dropWhile (not . null) $ lines contents
        final = fst $ foldl (\(g', p') d -> moveBlock g' p' d) (grid, findSub grid) moves
        wide = widen grid
        final' = fst $ foldl (\(g', p') d -> moveBlock g' p' d) (wide, findSub wide) moves
    printGrid' final
    print $ score final
    printGrid' final'
    print $ score final'

data Block = Empty | Wall | Box | Sub | LBox | RBox deriving (Show, Eq, Enum)

findSub g = head [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1], g!!y!!x == Sub]

parseGrid :: [String] -> [[Block]]
parseGrid g = [[ case c of '#' -> Wall; 'O' -> Box; '@' -> Sub; '.' -> Empty
                     | (c,x) <- zip row [0..]] | (row,y) <- zip g [0..]]
widen = map widenRow
widenRow [] = []
widenRow (b:r) = case b of 
                   Box -> LBox:RBox:wr
                   Sub -> Sub:Empty:wr
                   _ -> b:b:wr
                 where wr = widenRow r

add (x,y) (x',y') = (x+x',y+y')

moveBlock :: [[Block]] -> (Int, Int) -> Char -> ([[Block]], (Int, Int))
moveBlock g p@(x,y) d
  | next == Wall = (g, p)
  | next == Box || (next `elem` [LBox, RBox] && d `elem` ['>', '<']) =
      let (g', _) = moveBlock g nextP d in
          if g'!!ny!!nx == Empty then moveInto g' else (g, p)
  | next == LBox =
      let (g', _) = moveBlock g nextP d
          (g'', _) = moveBlock g' (nx+1,ny) d in
          if g''!!ny!!nx == Empty && g''!!ny!!(nx+1) == Empty then moveInto g'' else (g, p)
  | next == RBox =
      let (g', _) = moveBlock g nextP d
          (g'', _) = moveBlock g' (nx-1,ny) d in
          if g''!!ny!!nx == Empty && g''!!ny!!(nx-1) == Empty then moveInto g'' else (g, p)
  | next == Empty = moveInto g
  where delta = case d of
                  '^' -> (0, -1)
                  '>' -> (1, 0)
                  '<' -> (-1, 0)
                  'v' -> (0, 1)
        nextP@(nx,ny) = add p delta
        next = g!!ny!!nx
        curType = g!!y!!x
        moveInto gr = (updateGrid (updateGrid gr nextP curType) p Empty, nextP)

updateGrid :: [[a]] -> (Int, Int) -> a -> [[a]]
updateGrid grid (x, y) newValue =
    let (beforeRow, row:afterRow) = splitAt y grid
        (beforeElem, _:afterElem) = splitAt x row
    in beforeRow ++ (beforeElem ++ newValue : afterElem) : afterRow

btoc b = ['.', '#', 'O', '@', '[', ']']!!fromEnum b
printGrid' g = printGrid $ map (map btoc) g

score g = sum [x + 100*y |
                x <- [0..length (head g)-1],
                y <- [0..length g-1],
                g!!y!!x `elem` [Box, LBox]]
