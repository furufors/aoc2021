#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Maybe
import Data.List (reverse, sortBy)
import Debug.Trace (trace)
import Control.Parallel (par)
import qualified Data.Map as M

data Amphipod = E | A | B | C | D deriving (Eq, Ord)
data Position = H1 | H2 | H3 | H4 | H5 | H6 | H7 | A1 | A2 | A3 | A4 | B1 | B2 | B3 | B4 | C1 | C2 | C3 | C4 | D1 | D2 | D3 | D4 | DU deriving (Eq, Ord, Show)
data Hallway = Hallway {
    h1 :: Amphipod,
    h2 :: Amphipod,
    h3 :: Amphipod,
    h4 :: Amphipod,
    h5 :: Amphipod,
    h6 :: Amphipod,
    h7 :: Amphipod,
    a1 :: Amphipod,
    a2 :: Amphipod,
    a3 :: Amphipod,
    a4 :: Amphipod,
    b1 :: Amphipod,
    b2 :: Amphipod,
    b3 :: Amphipod,
    b4 :: Amphipod,
    c1 :: Amphipod,
    c2 :: Amphipod,
    c3 :: Amphipod,
    c4 :: Amphipod,
    d1 :: Amphipod,
    d2 :: Amphipod,
    d3 :: Amphipod,
    d4 :: Amphipod,
    du :: Amphipod
} deriving (Eq, Ord)
-- #############
-- #12.3.4.5.67#H
-- ###A#B#C#D###
--   #A#B#C#D#
--   #########

type State = (Int, Hallway)
type History = M.Map Hallway Int

-- Algorithm, take all valid next steps.
-- Remove Hallway states that has already been visited and is costlier
-- If a duplicate Hallway state occurs, update memory with the one with lowest score
main = putStrLn . show . minimum . map snd . filter (\(h,_) -> isDone h) . M.toList $ game M.empty [(0, startState)]

game :: History -> [State] -> History
game hist [    ] = hist
game hist states = let pq = sortBy (\a b -> compare (fst a) (fst b)) states -- exclude highest first
                       possibleNextSteps = nextSteps . head $ pq
                       (cheaperNextSteps, cheaperHist) = removeCostlier hist possibleNextSteps
                   in game cheaperHist (cheaperNextSteps ++ (tail pq))

removeCostlier :: History -> [State] -> ([State], History)
removeCostlier h [    ] = ([], h)
removeCostlier h (s:ss) = case M.lookup (snd s) h of
    Nothing -> let h' = M.insert (snd s) (fst s) h in (\(ss1, h1) -> (s:ss1, h1)) $ removeCostlier h' ss
    Just i  -> if fst s < i then let h' = M.insert (snd s) (fst s) h in (\(ss1, h1) -> (s:ss1, h1)) $ removeCostlier h' ss else removeCostlier h ss

nextSteps :: State -> [State]
nextSteps s = if isDone (snd s)
              then []
              else let ex1 = moveFromHallway s
                       ex2 = moveFromCaveA1 s ++ moveFromCaveB1 s
                       ex3 = moveFromCaveC1 s ++ moveFromCaveD1 s
                       ex4 = movea2 s ++ moveb2 s ++ movec2 s ++ moved2 s
                       ex5 = movea3 s ++ moveb3 s ++ movec3 s ++ moved3 s
                       ex6 = movea4 s ++ moveb4 s ++ movec4 s ++ moved4 s
                       --action = if ex1 /= []
                       --         then ex1
                       --         else ex2 `par` (ex3 `par` (ex4 `par` (ex5 `par` (ex6 `par` ex2 ++ ex3 ++ ex4 ++ ex5 ++ ex6)))) -- Prioritize moveFromHallway
                    in ex1 `par` (ex2 `par` (ex3 `par` (ex4 `par` (ex5 `par` (ex6 `par` ex1 ++ ex2 ++ ex3 ++ ex4 ++ ex5 ++ ex6)))))


-- Double (e.g.) H2 H2 to add correct distance when going from A1 -> H2
a1ToHallway = map ([A1] ++) [[DU, H2, H1], [DU, H2], [DU, H3], [DU, H3, DU, H4], [DU, H3, DU, H4, DU, H5], [DU, H3, DU, H4, DU, H5, DU, H6], [DU, H3, DU, H4, DU, H5, DU, H6, H7]]
b1ToHallway = map ([B1] ++) [[DU, H3, DU, H2, H1], [DU, H3, DU, H2], [DU, H3], [DU, H4], [DU, H4, DU, H5], [DU, H4, DU, H5, DU, H6], [DU, H4, DU, H5, DU, H6, H7]]
c1ToHallway = map ([C1] ++) [[DU, H4, DU, H3, DU, H2, H1], [DU, H4, DU, H3, DU, H2], [DU, H4, DU, H3], [DU, H4], [DU, H5], [DU, H5, DU, H6], [DU, H5, DU, H6, H7]]
d1ToHallway = map ([D1] ++) [[DU, H5, DU, H4, DU, H3, DU, H2, H1], [DU, H5, DU, H4, DU, H3, DU, H2], [DU, H5, DU, H4, DU, H3], [DU, H5, DU, H4], [DU, H5], [DU, H6], [DU, H6, H7]]
hallwayToA1 = map reverse a1ToHallway
hallwayToB1 = map reverse b1ToHallway
hallwayToC1 = map reverse c1ToHallway
hallwayToD1 = map reverse d1ToHallway
hallwayToA2 = map (++ [A2]) hallwayToA1
hallwayToB2 = map (++ [B2]) hallwayToB1
hallwayToC2 = map (++ [C2]) hallwayToC1
hallwayToD2 = map (++ [D2]) hallwayToD1
hallwayToA3 = map (++ [A3]) hallwayToA2
hallwayToB3 = map (++ [B3]) hallwayToB2
hallwayToC3 = map (++ [C3]) hallwayToC2
hallwayToD3 = map (++ [D3]) hallwayToD2
hallwayToA4 = map (++ [A4]) hallwayToA3
hallwayToB4 = map (++ [B4]) hallwayToB3
hallwayToC4 = map (++ [C4]) hallwayToC3
hallwayToD4 = map (++ [D4]) hallwayToD3

-- Hallway can only go directly home
moveFromHallway s = concat . map (\(a,p) -> moveToHome (a (snd s)) s p) $ zip [h1, h2, h3, h4, h5, h6, h7] [H1, H2, H3, H4, H5, H6, H7] 
moveFromCaveA1 (c, h) = if all (\a -> a h == A) [a1,a2,a3,a4] then [] else concat . map (moveAlongPath (c, h)) $ a1ToHallway 
moveFromCaveB1 (c, h) = if all (\a -> a h == B) [b1,b2,b3,b4] then [] else concat . map (moveAlongPath (c, h)) $ b1ToHallway 
moveFromCaveC1 (c, h) = if all (\a -> a h == C) [c1,c2,c3,c4] then [] else concat . map (moveAlongPath (c, h)) $ c1ToHallway 
moveFromCaveD1 (c, h) = if all (\a -> a h == D) [d1,d2,d3,d4] then [] else concat . map (moveAlongPath (c, h)) $ d1ToHallway 
movea2 (c, h) = if all (\a -> a h == A) [a2,a3,a4] then [] else moveAlongPath (c, h) [A2,A1]
moveb2 (c, h) = if all (\a -> a h == B) [b2,b3,b4] then [] else moveAlongPath (c, h) [B2,B1]
movec2 (c, h) = if all (\a -> a h == C) [c2,c3,c4] then [] else moveAlongPath (c, h) [C2,C1]
moved2 (c, h) = if all (\a -> a h == D) [d2,d3,d4] then [] else moveAlongPath (c, h) [D2,D1]
movea3 (c, h) = if all (\a -> a h == A) [a3,a4] then [] else if all (\a -> a h == E) [a1,a2] then moveAlongPath (c, h) [A3,A2,A1] else []
moveb3 (c, h) = if all (\a -> a h == B) [b3,b4] then [] else if all (\a -> a h == E) [b1,b2] then moveAlongPath (c, h) [B3,B2,B1] else []
movec3 (c, h) = if all (\a -> a h == C) [c3,c4] then [] else if all (\a -> a h == E) [c1,c2] then moveAlongPath (c, h) [C3,C2,C1] else []
moved3 (c, h) = if all (\a -> a h == D) [d3,d4] then [] else if all (\a -> a h == E) [d1,d2] then moveAlongPath (c, h) [D3,D2,D1] else []
movea4 (c, h) = if all (\a -> a h == A) [a4] then [] else if all (\a -> a h == E) [a1,a2,a3] then moveAlongPath (c, h) [A4,A3,A2,A1] else []
moveb4 (c, h) = if all (\a -> a h == B) [b4] then [] else if all (\a -> a h == E) [b1,b2,b3] then moveAlongPath (c, h) [B4,B3,B2,B1] else []
movec4 (c, h) = if all (\a -> a h == C) [c4] then [] else if all (\a -> a h == E) [c1,c2,c3] then moveAlongPath (c, h) [C4,C3,C2,C1] else []
moved4 (c, h) = if all (\a -> a h == D) [d4] then [] else if all (\a -> a h == E) [d1,d2,d3] then moveAlongPath (c, h) [D4,D3,D2,D1] else []

moveAlongPath :: State -> [Position] -> [State]
moveAlongPath (c, h) ps = let s = head ps
                              t = last ps
                              steps = map axs (tail ps)
                              as = (axs s) h
                          in if as /= E && pathClear (c, h) steps
                             then [(c + (length steps) * stepCost as, setTarget t as $ clearSource s h)]
                             else []

setTarget :: Position -> Amphipod -> Hallway -> Hallway
setTarget H1 a h = h { h1 = a }
setTarget H2 a h = h { h2 = a }
setTarget H3 a h = h { h3 = a }
setTarget H4 a h = h { h4 = a }
setTarget H5 a h = h { h5 = a }
setTarget H6 a h = h { h6 = a }
setTarget H7 a h = h { h7 = a }
setTarget A1 a h = h { a1 = a }
setTarget A2 a h = h { a2 = a }
setTarget A3 a h = h { a3 = a }
setTarget A4 a h = h { a4 = a }
setTarget B1 a h = h { b1 = a }
setTarget B2 a h = h { b2 = a }
setTarget B3 a h = h { b3 = a }
setTarget B4 a h = h { b4 = a }
setTarget C1 a h = h { c1 = a }
setTarget C2 a h = h { c2 = a }
setTarget C3 a h = h { c3 = a }
setTarget C4 a h = h { c4 = a }
setTarget D1 a h = h { d1 = a }
setTarget D2 a h = h { d2 = a }
setTarget D3 a h = h { d3 = a }
setTarget D4 a h = h { d4 = a }
setTarget DU a h = error "No traffic TO or from Dummy"

clearSource :: Position -> Hallway -> Hallway
clearSource H1 h = h { h1 = E }
clearSource H2 h = h { h2 = E }
clearSource H3 h = h { h3 = E }
clearSource H4 h = h { h4 = E }
clearSource H5 h = h { h5 = E }
clearSource H6 h = h { h6 = E }
clearSource H7 h = h { h7 = E }
clearSource A1 h = h { a1 = E }
clearSource A2 h = h { a2 = E }
clearSource A3 h = h { a3 = E }
clearSource A4 h = h { a4 = E }
clearSource B1 h = h { b1 = E }
clearSource B2 h = h { b2 = E }
clearSource B3 h = h { b3 = E }
clearSource B4 h = h { b4 = E }
clearSource C1 h = h { c1 = E }
clearSource C2 h = h { c2 = E }
clearSource C3 h = h { c3 = E }
clearSource C4 h = h { c4 = E }
clearSource D1 h = h { d1 = E }
clearSource D2 h = h { d2 = E }
clearSource D3 h = h { d3 = E }
clearSource D4 h = h { d4 = E }
clearSource DU h = error "No traffic to or FROM Dummy"

pathClear :: State -> [Hallway -> Amphipod] -> Bool
pathClear s ls = all (\f -> f (snd s) == E) ls

moveToHome :: Amphipod -> State -> Position -> [State]
moveToHome E  _        _ = []
moveToHome A s@(c, h) p = if all (\a -> a h == E)  [a1,a2,a3,a4]
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA4
                          else if all (\a -> a h == E)  [a1,a2,a3] || all (\a -> a h == A) [a4] 
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA3
                               else if all (\a -> a h == E)  [a1,a2] || all (\a -> a h == A) [a3, a4] 
                                    then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA2
                                    else if all (\a -> a h == E)  [a1] || all (\a -> a h == A) [a2,a3, a4] 
                                         then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA1
                                         else [] 
moveToHome B s@(c, h) p = if all (\a -> a h == E)  [b1,b2,b3,b4]
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB4
                          else if all (\a -> a h == E)  [b1,b2,b3] || all (\a -> a h == B) [b4] 
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB3
                               else if all (\a -> a h == E)  [b1,b2] || all (\a -> a h == B) [b3, b4] 
                                    then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB2
                                    else if all (\a -> a h == E)  [b1] || all (\a -> a h == B) [b2,b3, b4] 
                                         then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB1
                                         else [] 
moveToHome C s@(c, h) p = if all (\a -> a h == E)  [c1,c2,c3,c4]
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC4
                          else if all (\a -> a h == E)  [c1,c2,c3] || all (\a -> a h == C) [c4] 
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC3
                               else if all (\a -> a h == E)  [c1,c2] || all (\a -> a h == C) [c3, c4] 
                                    then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC2
                                    else if all (\a -> a h == E)  [c1] || all (\a -> a h == C) [c2,c3, c4] 
                                         then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC1
                                         else [] 
moveToHome D s@(c, h) p = if all (\a -> a h == E)  [d1,d2,d3,d4]
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToD4
                          else if all (\a -> a h == E)  [d1,d2,d3] || all (\a -> a h == D) [d4] 
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToD3
                               else if all (\a -> a h == E)  [d1,d2] || all (\a -> a h == D) [d3, d4] 
                                    then moveAlongPath s . head . filter ((==p) . head) $ hallwayToD2
                                    else if all (\a -> a h == E)  [d1] || all (\a -> a h == D) [d2,d3, d4] 
                                         then moveAlongPath s . head . filter ((==p) . head) $ hallwayToD1
                                         else [] 

axs :: Position -> (Hallway -> Amphipod)
axs H1 = h1
axs H2 = h2
axs H3 = h3
axs H4 = h4
axs H5 = h5
axs H6 = h6
axs H7 = h7
axs A1 = a1
axs A2 = a2
axs A3 = a3
axs A4 = a4
axs B1 = b1
axs B2 = b2
axs B3 = b3
axs B4 = b4
axs C1 = c1
axs C2 = c2
axs C3 = c3
axs C4 = c4
axs D1 = d1
axs D2 = d2
axs D3 = d3
axs D4 = d4
axs DU = du

isDone :: Hallway -> Bool
isDone h = a1 h == A && a2 h == A && a3 h == A && a4 h == A &&
           b1 h == B && b2 h == B && b3 h == B && b4 h == B && 
           c1 h == C && c2 h == C && c3 h == C && c4 h == C &&
           d1 h == D && d2 h == D && d3 h == D && d4 h == D

stepCost :: Amphipod -> Int
stepCost E = error "Trying to move nothing"
stepCost A = 1
stepCost B = 10
stepCost C = 100
stepCost D = 1000

startState :: Hallway
startState = Hallway {
    h1 = E,
    h2 = E,
    h3 = E,
    h4 = E,
    h5 = E,
    h6 = E,
    h7 = E,
    a1 = A,
    a2 = D,
    a3 = D,
    a4 = C,
    b1 = D,
    b2 = C,
    b3 = B,
    b4 = D,
    c1 = A,
    c2 = B,
    c3 = A,
    c4 = B,
    d1 = C,
    d2 = A,
    d3 = C,
    d4 = B,
    du = E
}

-- #############
-- #...........#
-- ###A#D#A#C###
--   #D#C#B#A#
--   #D#B#A#C#
--   #C#D#B#B#
--   #########

exampleState :: Hallway
exampleState = Hallway {
    h1 = E,
    h2 = E,
    h3 = E,
    h4 = E,
    h5 = E,
    h6 = E,
    h7 = E,
    a1 = B,
    a2 = D,
    a3 = D,
    a4 = A,
    b1 = C,
    b2 = C,
    b3 = B,
    b4 = D,
    c1 = B,
    c2 = B,
    c3 = A,
    c4 = C,
    d1 = D,
    d2 = A,
    d3 = C,
    d4 = A,
    du = E
}

-- #############
-- #...........#
-- ###B#C#B#D###
--   #D#C#B#A#
--   #D#B#A#C#
--   #A#D#C#A#
--   #########