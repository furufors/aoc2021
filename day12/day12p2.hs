#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort)
data Cave = Start | Large String | Small String | End deriving (Eq, Ord)
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
        addStep sys routes = uniq $ 
                             [if isDone route then route else direction e (head route):route
                             | route <- routes
                             , e <- sys
                             , isDone route || continuous e (head route)
                             , isDone route || visited ((direction e (head route)):route)
                             , isDone route || not (contains Start e)]
            where
                visited = (\x -> maxOneMulti x && noMoreThanDouble x) . map length . group . sort . toListOfSmallVisited
                maxOneMulti = (<2) . length . filter (>=2)
                noMoreThanDouble = all (<3)
                toListOfSmallVisited [        ] = []
                toListOfSmallVisited ((f, (Small t)):ss) = (Small t):toListOfSmallVisited ss
                toListOfSmallVisited ((f,         t):ss) = toListOfSmallVisited ss
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
        