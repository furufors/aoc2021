#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
import Data.List (sortBy)
import Data.Ord (comparing)

main = interact $ show . minfuel . calculate . parsein
    where
        minfuel :: [(Int,Int)] -> Int
        minfuel = snd . head . sortBy (comparing snd)
        calculate :: [Int] -> [(Int,Int)]
        calculate ps = map (\p -> (p, sum [abs (p-p') | p' <- ps])) [minimum ps..maximum ps]
        parsein :: String -> [Int]
        parsein = map read . words . map (\case ',' -> ' '; x -> x)
