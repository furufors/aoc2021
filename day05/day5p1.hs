#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Debug.Trace (trace)
type Line = ((Int, Int), (Int, Int)) -- ((x1, y1), (x2, y2))
type Map = [[Int]]

main :: IO ()
main = interact $ show . score . play . horv . parsein
    where
        play :: [Line] -> Map
        play ls = trace (show ls) $ foldl step newMap ls
            where
                extent :: [Line] -> Int
                extent                    [] = 1
                extent (((a, b), (c, d)):xs) = maximum $ [a,b,c,d,extent xs]
                size :: Int
                size = extent ls
                step :: Map -> Line -> Map
                step m ((a, b), (c,d)) =
                    let n = [[if x >= min a c && x <=max a c && y >= min b d && y <= max b d then 1 else 0| x <- [0..size]] | y <- [0..size]]
                    in zipWith (zipWith (+)) m n
                newMap :: Map
                newMap = [[0 | _ <- [0..size]] | _ <- [0..size]]
        
        score :: Map -> Int
        score = sum . map sum . (map (map (\x -> if x > 1 then 1 else 0 )))

        horv :: [Line] -> [Line]
        horv = filter (\l -> h l || v l)
            where
                h ((a,_),(c,_)) = a == c
                v ((_,b),(_,d)) = b == d

        parsein :: String -> [Line]
        parsein = map (\x -> ((x!!0, x!!1), (x!!2, x!!3))) . map (map read . words) . lines . map repl
                  where
                    repl ',' = ' '
                    repl '-' = ' '
                    repl '>' = ' '
                    repl  x  =  x
