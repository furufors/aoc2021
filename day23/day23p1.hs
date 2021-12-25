#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Maybe
import qualified Data.Map as M

data Amphipod = A | B | C | D deriving Eq
data Hallway = Hallway {
    h1 :: Maybe Amphipod,
    h2 :: Maybe Amphipod,
    h3 :: Maybe Amphipod,
    h4 :: Maybe Amphipod,
    h5 :: Maybe Amphipod,
    h6 :: Maybe Amphipod,
    h7 :: Maybe Amphipod,
    a1 :: Maybe Amphipod,
    a2 :: Maybe Amphipod,
    b1 :: Maybe Amphipod,
    b2 :: Maybe Amphipod,
    c1 :: Maybe Amphipod,
    c2 :: Maybe Amphipod,
    d1 :: Maybe Amphipod,
    d2 :: Maybe Amphipod
} {-
#############
#12.3.4.5.67#H
###A#D#A#C###
  #C#D#B#B#
  #########
-}
type State = (Int, Hallway)
type History = M.Map State Int

-- Algorithm, take all valid next steps.
-- Remove Hallway states that has already been visited and is costlier
-- If a duplicate Hallway state occurs, update memory with the one with lowest score
main = show . minimum . map fst . filter (\(c,h) -> isDone h) $ game M.empty (0, startState)

game :: History -> State -> History
game hist state = let possibleNextSteps = nextSteps state
                      (cheaperNextSteps, cheaperHist) = removeCostlier hist possibleNextSteps
                  in foldl game cheaperHist cheaperNextSteps

cheaperNextSteps :: History -> [State] -> ([State], History)
cheaperNextSteps hist [        ] = ([], hist)
cheaperNextSteps hist ((c,h):ss) = case M.lookup h hist of
    Nothing -> \(h', cs) -> (h', (c,h):ss) $ cheaperNextSteps hist ss
    Just c' -> if c' <= c then cheaperNextSteps hist ss else let hist' = M.insert c h hist in \(h', cs) -> (h', (c,h):ss) $ cheaperNextSteps hist' ss

nextSteps :: State -> [State]
nextSteps s = moveh1 s ++ moveh2 s ++ moveh3 s ++ moveh4 s ++ moveh5 s ++ moveh6 s ++ moveh7 s ++
              movea1 s ++ movea2 s ++ moveb1 s ++ moveb2 s ++ movec1 s ++ movec2 s ++ moved1 s ++ moved2 s

moveh1 (c, h) = let ah1 = h1 h; ah2 = h2 h
                    right = if ah2 == Nothing                  then [(c + 1 * stepCost ah1, h { h1 = Nothing, h2 = ah1 })] else []
                in if ah1 /= Nothing then right else []
moveh2 (c, h) = let ah2 = h2 h; ah1 = h1 h; ah3 = h3 h; aa1 = a1 h
                    left  = if ah1 == Nothing                  then [(c + 1 * stepCost ah2, h { h2 = Nothing, h1 = ah2 })] else []
                    rdown = if aa1 == Nothing                  then [(c + 2 * stepCost ah2, h { h2 = Nothing, a1 = ah2 })] else []
                    right = if ah3 == Nothing && ah2 == Just A then [(c + 2 * stepCost ah2, h { h2 = Nothing, h3 = ah2 })] else []
                in if ah2 /= Nothing then left ++ right ++ rdown else []
moveh3 (c, h) = let ah3 = h3 h; ah4 = h4 h; ah2 = h2 h; aa1 = a1 h; ab1 = b1 h
                    left  = if ah2 == Nothing                  then [(c + 2 * stepCost ah3, h { h3 = Nothing, h2 = ah3 })] else []
                    ldown = if aa1 == Nothing && ah3 == Just A then [(c + 2 * stepCost ah3, h { h3 = Nothing, a1 = ah3 })] else []
                    rdown = if ab1 == Nothing && ah3 == Just B then [(c + 2 * stepCost ah3, h { h3 = Nothing, b1 = ah3 })] else []
                    right = if ah4 == Nothing                  then [(c + 2 * stepCost ah3, h { h3 = Nothing, h4 = ah3 })] else []
                in if ah3 /= Nothing then left ++ right ++ ldown ++ rdown else []
moveh4 (c, h) = let ah4 = h4 h; ah5 = h5 h; ah3 = h3 h; ab1 = b1 h; ac1 = c1 h
                    left  = if ah3 == Nothing                  then [(c + 2 * stepCost ah4, h { h4 = Nothing, h3 = ah4 })] else []
                    ldown = if ab1 == Nothing && ah4 == Just B then [(c + 2 * stepCost ah4, h { h4 = Nothing, b1 = ah4 })] else []
                    rdown = if ac1 == Nothing && ah4 == Just C then [(c + 2 * stepCost ah4, h { h4 = Nothing, c1 = ah4 })] else []
                    right = if ah5 == Nothing                  then [(c + 2 * stepCost ah4, h { h4 = Nothing, h5 = ah4 })] else []
                in if ah4 /= Nothing then left ++ right ++ ldown ++ rdown else []
moveh5 (c, h) = let ah5 = h5 h; ah6 = h6 h; ah4 = h4 h; ac1 = c1 h; ad1 = d1 h
                    left  = if ah4 == Nothing                  then [(c + 2 * stepCost ah5, h { h5 = Nothing, h4 = ah5 })] else []
                    ldown = if ac1 == Nothing && ah5 == Just C then [(c + 2 * stepCost ah5, h { h5 = Nothing, c1 = ah5 })] else []
                    rdown = if ad1 == Nothing && ah5 == Just D then [(c + 2 * stepCost ah5, h { h5 = Nothing, d1 = ah5 })] else []
                    right = if ah6 == Nothing                  then [(c + 2 * stepCost ah5, h { h5 = Nothing, h6 = ah5 })] else []
                in if ah5 /= Nothing then left ++ right ++ ldown ++ rdown else []
moveh6 (c, h) = let ah6 = h6 h; ah7 = h7 h; ah5 = h5 h; ad1 = d1 h
                    left  = if ah5 == Nothing                  then [(c + 2 * stepCost ah6, h { h6 = Nothing, h5 = ah6 })] else []
                    ldown = if ad1 == Nothing && ah5 == Just D then [(c + 2 * stepCost ah6, h { h6 = Nothing, d1 = ah6 })] else []
                    right = if ah7 == Nothing                  then [(c + 1 * stepCost ah6, h { h6 = Nothing, h7 = ah6 })] else []
                in if ah6 /= Nothing then left ++ right ++ ldown ++ rdown else []
moveh7 (c, h) = let ah7 = h7 h; ah6 = h6 h
                    left  = if ah6 == Nothing                  then [(c + 1 * stepCost ah7, h { h7 = Nothing, h6 = ah7 })] else []
                in if ah7 /= Nothing then left else []
movea1 (c, h) = let ah2 = h2 h; ah3 = h3 h 


isDone :: Hallway -> Bool
isDone h = a1 h == Just A && a2 h == Just A &&
           b1 h == Just B && b2 h == Just B &&
           c1 h == Just C && c2 h == Just C &&
           d1 h == Just D && d2 h == Just D

stepCost :: Mayve Amphipod -> Int
stepCost Nothing = error "Trying to move nothing"
stepCost Just A = 1
stepCost Just B = 10
stepCost Just C = 100
stepCost Just D = 1000

startState :: Hallway
startState = Hallway {
    h1 = Nothing,
    h2 = Nothing,
    h3 = Nothing,
    h4 = Nothing,
    h5 = Nothing,
    h6 = Nothing,
    h7 = Nothing,
    a1 = Just A,
    a2 = Just C,
    b1 = Just D,
    b2 = Just D,
    c1 = Just A,
    c2 = Just B,
    d1 = Just C,
    d2 = Just B,
    cost :: 0
}
