#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
type Area = [[Char]]

main = interact $ show . converge 0 step . lines
    where
        converge :: Eq a => Int -> (a -> a) -> a -> Int
        converge n f a = if f a == a then n + 1 else converge (n+1) f (f a)

        step :: Area -> Area
        step = stepSouth . stepEast

        stepEast :: Area -> Area
        stepEast a = let width = length (head a) - 1
                         height = length a - 1 
                     in [[if left == '>' && this == '.' then '>' else if right == '.' && this == '>' then '.' else this
                         | x <- [0..width]
                         , let leftX = mod (x-1) (width + 1)
                         , let rightX = mod (x+1) (width + 1)
                         , let this = a!!y!!x
                         , let left = a!!y!!leftX
                         , let right = a!!y!!rightX
                         ] | y <- [0..height]
                        ]

        stepSouth :: Area -> Area
        stepSouth a = let width = length (head a) - 1
                          height = length a - 1 
                      in [[if up == 'v' && this == '.' then 'v' else if down == '.' && this == 'v' then '.' else this 
                          | x <- [0..width]
                          , let upY = mod (y-1) (height + 1)
                          , let downY = mod (y+1) (height + 1)
                          , let this = a!!y!!x
                          , let up = a!!upY!!x
                          , let down = a!!downY!!x
                          ] | y <- [0..height]
                         ]