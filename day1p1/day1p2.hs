#!/usr/bin/env stack
-- stack --resolver lts-13.7 script
main :: IO ()
main = interact $ show . trues . increasing . map tripletsum . triplets . parsein
    where
        trues = length . filter id
        increasing l = zipWith (>) (drop 1 l) l
        tripletsum (a,b,c) = a + b + c
        triplets l = zip3 (drop 2 l) (drop 1 l) l
        parsein = map (\x -> read x :: Int) . lines