#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort, reverse)
import Debug.Trace (trace)
data Cave = Start | Large String | Small String | End deriving (Eq, Ord)
instance Show Cave where
    show Start = "Start"
    show (Small a) =  a
    show (Large a) =  a
    show End = "End"
type Step = (Cave, Cave) -- (From, To)

main = interact $ show . length . generatePaths . parsein
    where
        parsein :: String -> [Step]
        parsein = (map $ (\(a,b) -> makeStep (a, (tail b))) . span (/= '-')) . lines

        generatePaths :: [Step] -> [[Step]]
        generatePaths sys = converge (==) . iterate (addStep sys) $ getStart sys

        converge :: (a -> a -> Bool) -> [a] -> a
        converge p (x:ys@(y:_))
            | p x y     = y
            | otherwise = converge p ys

        getStart :: [Step] -> [[Step]]
        getStart [    ] = []
        getStart (x:xs) = if contains Start x then [x]:getStart xs else getStart xs

        addStep :: [Step] -> [[Step]] -> [[Step]]
        -- addStep sys routes = trace (showPaths routes) $ uniq $ 
        addStep sys routes = uniq $ 
                             [if isDone route then route else direction e (head route):route
                             | route <- routes
                             , e <- sys
                             , isDone route || continuous e (head route)
                             , isDone route || notVisited route (direction e (head route)) || targetIsLarge (direction e (head route))
                             , isDone route || not (contains Start e)]
            where
                notVisited ss (_,t) = not (any (contains t) ss)
                targetIsLarge (_, Large _) = True
                targetIsLarge (_, _      ) = False
                uniq = map head . group . sort
                continuous (a,b) (_,d) = elem d [a,b]
                isDone = (==End) . snd . head
                direction (f2, t2) (_,t1) = if t1 == f2 then (f2, t2) else (t2, f2)

        contains :: Cave -> Step -> Bool
        contains e (a, b) = a == e || b == e

        makeStep :: (String, String) -> Step
        makeStep (a,b) = let a' = makeStep' a; b' = makeStep' b
                         in if b' == Start
                            then (Start, a')
                            else if a' == End
                                 then (b', End) else (a', b') 
            where
                makeStep' "end"   = End
                makeStep' "start" = Start
                makeStep' x       = if head x `elem` ['A'..'Z'] then Large x else Small x

        showPaths :: [[Step]] -> String
        showPaths = ("-----\n"++) . foldl1 (\a b -> a ++ "\n" ++ b) . map (foldl1 (\a b -> a ++ "->" ++ b) . map show . reverse)
        