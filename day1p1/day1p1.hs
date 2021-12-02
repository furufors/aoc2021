#!/usr/bin/env stack
-- stack --resolver lts-13.7 script
main :: IO ()
main = interact $ show . length . filter (id) . (\l -> zipWith (>) (drop 1 l) l) . map (\x -> read x :: Int) . lines