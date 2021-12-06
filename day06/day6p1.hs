#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
type Fish = Int
type School = [Fish]
main :: IO ()
main = interact $ show . life 80 . parsein
    where
        life :: Int -> School -> Int
        life 0 fish = length fish
        life n fish = life (n-1) (aDayInLife fish)
        aDayInLife :: School -> School
        aDayInLife fish = map (\case 0 -> 6; x -> x - 1) fish ++ take (length . filter (==0) $ fish) (repeat 8)
        parsein :: String -> School
        parsein = map read . words . map (\case ',' -> ' '; x -> x)
