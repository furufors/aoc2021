#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Maybe
import Data.List (reverse)
import Debug.Trace (trace)
import qualified Data.Map as M

data Amphipod = E | A | B | C | D deriving (Eq, Ord)
data Position = H1 | H2 | H3 | H4 | H5 | H6 | H7 | A1 | A2 | B1 | B2 | C1 | C2 | D1 | D2 deriving (Eq, Ord, Show)
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
    b1 :: Amphipod,
    b2 :: Amphipod,
    c1 :: Amphipod,
    c2 :: Amphipod,
    d1 :: Amphipod,
    d2 :: Amphipod
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
main = putStrLn . show . minimum . map snd . filter (\(h,_) -> isDone h) . M.toList $ game M.empty (0, startState)

game :: History -> State -> History
game hist state = let possibleNextSteps = nextSteps state
                      (cheaperNextSteps, cheaperHist) = removeCostlier hist possibleNextSteps
                  in if cheaperNextSteps == []
                     then cheaperHist
                     else foldl game cheaperHist cheaperNextSteps

removeCostlier :: History -> [State] -> ([State], History)
removeCostlier h [    ] = ([], h)
removeCostlier h (s:ss) = case M.lookup (snd s) h of
    Nothing -> let h' = M.insert (snd s) (fst s) h in (\(ss1, h1) -> (s:ss1, h1)) $ removeCostlier h' ss
    Just i  -> if fst s < i then let h' = M.insert (snd s) (fst s) h in (\(ss1, h1) -> (s:ss1, h1)) $ removeCostlier h' ss else removeCostlier h ss

nextSteps :: State -> [State]
nextSteps s = if isDone (snd s)
              then []
              else moveFromHallway s ++ moveFromCave1 s ++ movea2 s ++ moveb2 s ++ movec2 s ++ moved2 s

-- Double (e.g.) H2 H2 to add correct distance when going from A1 -> H2
a1ToHallway = map ([A1] ++) [[H2, H2, H1], [H2, H2], [H3, H3], [H3, H3, H4, H4], [H3, H3, H4, H4, H5, H5], [H3, H3, H4, H4, H5, H5, H6, H6], [H3, H3, H4, H4, H5, H5, H6, H6, H7]]
b1ToHallway = map ([B1] ++) [[H3, H3, H2, H2, H1], [H3, H3, H2, H2], [H3, H3], [H4, H4], [H4, H4, H5, H5], [H4, H4, H5, H5, H6, H6], [H4, H4, H5, H5, H6, H6, H7]]
c1ToHallway = map ([C1] ++) [[H4, H4, H3, H3, H2, H2, H1], [H4, H4, H3, H3, H2, H2], [H4, H4, H3, H3], [H4, H4], [H5, H5], [H5, H5, H6, H6], [H5, H5, H6, H6, H7]]
d1ToHallway = map ([D1] ++) [[H5, H5, H4, H4, H3, H3, H2, H2, H1], [H5, H5, H4, H4, H3, H3, H2, H2], [H5, H5, H4, H4, H3, H3], [H5, H5, H4, H4], [H5, H5], [H6, H6], [H6, H6, H7]]
-- a1ToHallway = map ([A1] ++) [[H2, H1], [H2], [H3], [H3, H4], [H3, H4, H5], [H3, H4, H5, H6], [H3, H4, H5, H6, H7]]
-- b1ToHallway = map ([B1] ++) [[H3, H2, H1], [H3, H2, H2], [H3], [H4], [H4, H5], [H4, H5, H6], [H4, H5, H6, H7]]
-- c1ToHallway = map ([C1] ++) [[H4, H3, H2, H1], [H4, H3, H2, H2], [H4, H3], [H4], [H5], [H5, H6], [H5, H6, H7]]
-- d1ToHallway = map ([D1] ++) [[H5, H4, H3, H2, H1], [H5, H4, H3, H2], [H5, H4, H3], [H5, H4], [H5], [H6], [H6, H7]]
hallwayToA1 = map reverse a1ToHallway
hallwayToB1 = map reverse b1ToHallway
hallwayToC1 = map reverse c1ToHallway
hallwayToD1 = map reverse d1ToHallway
hallwayToA2 = map (++ [A2]) hallwayToA1
hallwayToB2 = map (++ [B2]) hallwayToB1
hallwayToC2 = map (++ [C2]) hallwayToC1
hallwayToD2 = map (++ [D2]) hallwayToD1

-- Hallway can only go directly home
moveFromHallway s = concat . map (\(a,p) -> moveToHome (a (snd s)) s p) $ zip [h1, h2, h3, h4, h5, h6, h7] [H1, H2, H3, H4, H5, H6, H7] 
moveFromCave1 s = concat . map (moveAlongPath s) $ a1ToHallway ++ b1ToHallway ++ c1ToHallway ++ d1ToHallway
movea2 (c, h) = if a2 h == A then [] else moveAlongPath (c, h) [A2,A1]
moveb2 (c, h) = if b2 h == B then [] else moveAlongPath (c, h) [B2,B1]
movec2 (c, h) = if c2 h == C then [] else moveAlongPath (c, h) [C2,C1]
moved2 (c, h) = if d2 h == D then [] else moveAlongPath (c, h) [D2,D1]

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
setTarget B1 a h = h { b1 = a }
setTarget B2 a h = h { b2 = a }
setTarget C1 a h = h { c1 = a }
setTarget C2 a h = h { c2 = a }
setTarget D1 a h = h { d1 = a }
setTarget D2 a h = h { d2 = a }

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
clearSource B1 h = h { b1 = E }
clearSource B2 h = h { b2 = E }
clearSource C1 h = h { c1 = E }
clearSource C2 h = h { c2 = E }
clearSource D1 h = h { d1 = E }
clearSource D2 h = h { d2 = E }

pathClear :: State -> [Hallway -> Amphipod] -> Bool
pathClear s ls = all (\f -> f (snd s) == E) ls

moveToHome :: Amphipod -> State -> Position -> [State]
moveToHome E  _        _ = []
moveToHome A s@(c, h) p = if a1 h == E && a2 h == E
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA2 
                          else if a2 h == A
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToA1
                               else []
moveToHome B s@(c, h) p = if b1 h == E && b2 h == E
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB2
                          else if b2 h == B
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToB1
                               else []
moveToHome C s@(c, h) p = if c1 h == E && c2 h == E
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC2
                          else if c2 h == C
                               then moveAlongPath s . head . filter ((==p) . head) $ hallwayToC1
                               else []
moveToHome D s@(c, h) p = if d1 h == E && d2 h == E
                          then moveAlongPath s . head . filter ((==p) . head) $ hallwayToD2
                          else if d2 h == D
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
axs B1 = b1
axs B2 = b2
axs C1 = c1
axs C2 = c2
axs D1 = d1
axs D2 = d2

isDone :: Hallway -> Bool
isDone h = a1 h == A && a2 h == A &&
           b1 h == B && b2 h == B &&
           c1 h == C && c2 h == C &&
           d1 h == D && d2 h == D

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
    a2 = C,
    b1 = D,
    b2 = D,
    c1 = A,
    c2 = B,
    d1 = C,
    d2 = B
}

-- #############
-- #...........#
-- ###A#D#A#C###
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
    a2 = A,
    b1 = C,
    b2 = D,
    c1 = B,
    c2 = C,
    d1 = D,
    d2 = A
}

-- #############
-- #...........#
-- ###B#C#B#D###
--   #A#D#C#A#
--   #########