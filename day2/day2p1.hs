#!/usr/bin/env stack
-- stack --resolver lts-13.7 script
type Step = (Int, Int)

main :: IO ()
main = interact $ show . multiply . integrate . parsein
    where
        multiply :: Step -> Int
        multiply (x,y) = x * y
        integrate :: [Step] -> Step
        integrate = foldl step (0,0)
        step :: Step -> Step -> Step
        step (a,b) (c,d) = (a + c, b + d)
        parsein :: String -> [Step]
        parsein = map parseCommand . lines
        parseCommand :: String -> Step
        parseCommand ('u':'p':' ':x) = (0, -1 * read x)
        parseCommand ('d':'o':'w':'n':' ':x) = (0, read x)
        parseCommand ('f':'o':'r':'w':'a':'r':'d':' ':x) = (read x, 0)
        parseCommand otherwise = error "bad input"