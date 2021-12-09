#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (chunksOf)

main = interact $ show . riskRating . pad . parsein
    where
        parsein :: String -> ((Int,Int), [[Int]])
        parsein = (\x -> ((length x, length (head x)),x)) . map (map read . chunksOf 1) . lines
        pad :: ((Int,Int), [[Int]]) -> ((Int,Int), [[Int]])
        pad ((h,w),m) = ((h,w), horiz ++ map (\x -> [9] ++ x ++ [9]) m ++ horiz)
            where horiz = [take (w+2) (repeat 9)]
        riskRating :: ((Int, Int), [[Int]]) -> Int
        riskRating ((h,w),m) = sum [riskRating2 m (y+1) (x+1) | x <- [0..w-1], y <- [0..h-1]]
        riskRating2 :: [[Int]] -> Int -> Int -> Int
        riskRating2 m y x = let v = m!!y!!x in
            if all (>v) [m!!(y-1)!!x, m!!(y+1)!!x, m!!y!!(x-1), m!!y!!(x+1)] then v+1 else 0
