#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{- Alternate solution based on code by topaz@github, added some parallelism and restructured to my liking. -}
import Algorithm.Search (dijkstra)
import Control.Parallel (par)
import Data.Maybe (fromMaybe)
{- #01#2#3#4#56#
   #..o.o.o.o..#
   ###A#C#B#C###
      0 1 2 3    -}
data Amphipod = E | A | B | C | D deriving (Eq,Ord,Show)
type Room = [Amphipod]
type Hall = [Amphipod]
type State = ([Room], Hall)
type Cost = Int

main :: IO ()
main = let p1 = solve [[A,C], [D,D], [A,B], [C,B]]
           p2 = solve [[A,D,D,C], [D,C,B,D], [A,B,A,B], [C,A,C,B]]
           res = p1 `par` (p2 `par` "P1: " ++ show p1 ++ "\n" ++ "P2: " ++ show p2)
       in putStrLn res

solve :: [Room] -> Cost
solve rooms = fst . fromMaybe (error "No solution") $ dijkstra neighbors (transitionCost l) (solved l) (rooms, [E,E,E,E,E,E,E])
  where
    l = length $ head rooms
    
    neighbors :: State -> [State]
    neighbors (rooms, hall) = intoHall `par` (outOfHall `par` intoHall ++ outOfHall)
      where
        intoHall  = [(rs', h')
                    | i <- [0..3]
                    , let room = getRoom rooms i
                    , not (null room)
                    , room /= replicate (length room) ([A,B,C,D] !! i)
                    , let (a:as) = room
                    , j <- [0..6]
                    , reachable hall i j
                    , let rs' = [if k == i then as else getRoom rooms k | k <- [0..3]]
                    , let h' = replace hall j a
                    ]
        outOfHall = [(rs', h')
                    | i <- [0..6]
                    , let a = hall !! i
                    , a /= E
                    , let j = roomIndex a
                    , let targetRoom = rooms !! j
                    , all (== a) targetRoom
                    , canReachHome hall i j
                    , let r' = a:targetRoom
                    , let rs' = replace rooms j r'
                    , let h' = replace hall i E
                    ]

        canReachHome :: Hall -> Int -> Int -> Bool
        canReachHome hall h r
          | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
          | r + 1 >= h + 1 = all (\i -> hall !! i == E) [h+1 .. r+1]
          | h - 1 >= r + 2 = all (\i -> hall !! i == E) [r+2 .. h-1]
          | otherwise      = True

        roomIndex :: Amphipod -> Int
        roomIndex A = 0
        roomIndex B = 1
        roomIndex C = 2
        roomIndex D = 3
        roomIndex E = error "Unknown room"

        reachable :: Hall -> Int -> Int -> Bool
        reachable hall r h
          | r `elem` [0..3] && h `elem` [0..6] = let left = [h..r+1]; right = [r+2..h]
                                                 in all (\i -> hall !! i == E) $ if r+1 >= h then left else right 
          | otherwise = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)

        replace :: [a] -> Int -> a -> [a]
        replace xs i c = [if j == i then c else x | (j, x) <- zip [0..] xs]

        getRoom :: [Room] -> Int -> Room
        getRoom rooms i = if i `elem` [0..3] then rooms !! i else error "Accessing non existing room"

    transitionCost :: Int -> State -> State -> Cost
    transitionCost len (rs, h) (rs', h') = costMove len (rs, h) roomNr hallNr
      where
        firstDifference :: Eq a => [a] -> [a] -> Int
        firstDifference xs ys = head [i | i <- [0..length xs], xs !! i /= ys !! i]
        roomNr = firstDifference rs rs'
        hallNr = firstDifference h h'

        costMove :: Int -> State -> Int -> Int -> Cost
        costMove len (rooms,hall) roomNr hallNr = (pathCost !! roomNr !! hallNr + offset) * moveCost a
          where
            as = rooms !! roomNr
            a  = if hall !! hallNr /= E then hall !! hallNr else head as
            offset = if hall !! hallNr /= E then len - 1 - length as else len - length as

            pathCost :: [[Cost]]
            pathCost = [[3,2,2,4,6,8,9]
                       ,[5,4,2,2,4,6,7]
                       ,[7,6,4,2,2,4,5]
                       ,[9,8,6,4,2,2,3]
                       ]

            moveCost :: Amphipod -> Int
            moveCost A = 1
            moveCost B = 10
            moveCost C = 100
            moveCost D = 1000
            moveCost E = error "Trying to move empty"

    solved :: Int -> State -> Bool
    solved len st = fst st == map (replicate len) [A,B,C,D]
