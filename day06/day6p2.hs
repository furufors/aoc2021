#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}

main = interact $ show . sum
                . (\x -> (iterate (\(f:fs) -> zipWith (+) (fs ++ [f]) (0:0:0:0:0:0:f:0:0:[])) x)!!256)
                . (\f -> [length . filter (== x) $ f | x <- [0..8]]) . map read . words . map (\case ',' -> ' '; x -> x)

-- main = interact $ show . life 256 . count . parsein
--     where
--         life 0 = sum 
--         life n = life (n-1) . aDayInLife 
--         aDayInLife fish = zipWith (+) (tail fish ++ [head fish]) (0:0:0:0:0:0:head fish:0:0:[])
--         count fish = [length . filter (== x) $ fish | x <- [0..8]] 
--         parsein :: String -> [Int]
--         parsein = 
