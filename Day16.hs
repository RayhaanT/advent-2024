import Util
import Data.List
import Data.Maybe
import Data.Bifunctor
import qualified Data.Heap as P
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Control.Monad
import Control.Monad.State

main = do
    contents <- readDay 16
    let grid = lines contents
        start = findChar 'S' grid
        end = findChar 'E' grid
    printGrid grid
    let res = dists $ execState (dijkstra grid) $
            DState (P.fromList [Candidate (Step start Rght) (Step (0,0) Up) 0]) S.empty M.empty
    print $ map fst $ lookupBest end res
    print $ length $ nub $ map spos $ concatMap ((trail res . Step end) . snd) (lookupBest end res)

spos (Step p _) = p

lookupBest p m = filter (\c -> fst c == best) cands
  where cands = lookupAll p m
        best = fst $ minimumBy (\(a,_) (b,_) -> compare a b) cands

lookupAll :: Pos -> M.Map Step (Int, [Step]) -> [(Int, Direction)]
lookupAll p m = mapMaybe (\d -> fmap ((,d) . fst) $ (`M.lookup` m) $ Step p d) [Up .. Rght]

findChar :: Eq a => a -> [[a]] -> (Int, Int)
findChar c g = head $ [(x,y) | x <- [0..length (head g)-1], y <- [0..length g-1], (g!!y)!!x == c]

trail :: M.Map Step (Int, [Step]) -> Step -> [Step]
trail m p
  | [Step (0,0) Up] <- pred = [p]
  | otherwise = p:concatMap (trail m) pred
  where (_,pred) = fromJust $ M.lookup p m

data Direction = Up | Down | Lft | Rght deriving (Show, Eq, Enum, Ord)
delta dir = [(0,-1),(0,1),(-1,0),(1,0)] !! fromEnum dir

type Pos = (Int, Int)
data Step = Step Pos Direction deriving (Show, Eq, Ord)
data Candidate = Candidate {
    pos :: Step,
    pred :: Step,
    cost :: Int
} deriving (Show, Eq)

instance Ord Candidate where
  compare (Candidate _ _ d) (Candidate _ _ d') = compare d d'

data DState = DState {
    open :: P.Heap Candidate,
    closed :: S.Set Step,
    dists :: M.Map Step (Int, [Step])
} deriving Show

valid g (x,y) = x >= 0 && y >= 0 && y < length g && x < length (head g)
space g (x,y) = (g!!y)!!x /= '#'
add (a,b) (c,d) = (a+c, b+d)

putIf c t f = if c then put t else put f

dijkstra :: [String] -> State DState ()
dijkstra g = do
    s <- get
    case P.uncons $ open s of
      Nothing -> return ()
      Just (Candidate st@(Step p dir) pred d, op) -> do
          putIf (S.member st (closed s))
            (DState op (closed s)
                (if d == fst (fromJust $ M.lookup st (dists s))
                   then M.alter (fmap (second (pred :))) st (dists s)
                   else dists s
                   ))
            (DState (foldl (flip P.insert) op cands)
                (S.insert st $ closed s)
                (M.insert st (d, [pred]) $ dists s))
          dijkstra g
              where nextp = add p $ delta dir
                    next = if valid g nextp && space g nextp
                              then Just $ Candidate (Step nextp dir) st (d+1)
                              else Nothing
                    turns = [Candidate (Step p dir') st (d+1000) |
                                dir' <- [Up .. Rght], dir' /= dir]
                    cands = catMaybes (next : map Just turns)
