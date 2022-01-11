#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
import Data.List (sortBy)
import Data.Ord (comparing)

main = interact $ show . minfuel . calculate . parsein
    where
        minfuel :: [(Int,Int)] -> Int
        minfuel = snd . head .sortBy (comparing snd)
        calculate :: [Int] -> [(Int,Int)]
        calculate ps = map (\p -> (p, sum [cost (abs (p-p')) | p' <- ps])) [minimum ps..maximum ps]
        cost :: Int -> Int
        cost 0 = 0
        cost n = n + cost (n-1)
        parsein :: String -> [Int]
        parsein = map read . words . map (\case ',' -> ' '; x -> x)
