#!/usr/bin/env stack
-- stack --resolver lts-13.7 script
type Aim = Integer
type Horizontal = Integer
type Depth = Integer
type Target = (Aim, Horizontal, Depth)
data Step = Down Integer | Up Integer | Forward Integer

main :: IO ()
main = interact $ show . multiply . integrate . parsein
    where
        multiply :: Target -> Integer
        multiply (_, h, d) = h * d
        integrate :: [Step] -> Target
        integrate = foldl step (0, 0, 0)
        step :: Target -> Step -> Target
        step (a, h, d) (Down    x) = (a + x,     h,         d)
        step (a, h, d) (Up      x) = (a - x,     h,         d)
        step (a, h, d) (Forward x) = (    a, h + x, d + a * x)
        parsein :: String -> [Step]
        parsein = map parseStep . lines
        parseStep :: String -> Step
        parseStep ('u':'p':' ':x) = Up $ read x
        parseStep ('d':'o':'w':'n':' ':x) = Down $ read x
        parseStep ('f':'o':'r':'w':'a':'r':'d':' ':x) = Forward $ read x
        parseStep otherwise = error "bad input"
        