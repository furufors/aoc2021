#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (sort)
type Prog = [Inst]
data Inst = Choice Integer | Given Integer

main :: IO ()
main = putStrLn . show . solve $ task
    where
        -- Choice n: zr = 26 * z0 + w + n
        -- Given  n: zr = (z0 div 26) when z0 mod 26 - n == w
        task = [ Choice  6
               , Choice  6
               , Choice  3
               , Given  11
               , Choice  9
               , Given   1
               , Choice 13
               , Choice  6
               , Given   0
               , Choice 10
               , Given   5
               , Given  16
               , Given   7
               , Given  11
               ]

        solve :: Prog -> Integer
        solve p = head . sort . map (sum . zipWith (\a b -> 10^a * b) [13,12..0] . snd) . filter ((==0) . fst) $ step p [(0,[])]

        step :: Prog -> [(Integer, [Integer])] -> [(Integer, [Integer])]
        step [             ] gs = gs
        step ((Choice n):ps) gs = step ps [(26*z0 + n + w, ws ++ [w]) | (z0, ws) <- gs, w <- [1..9]]
        step ((Given  n):ps) gs = step ps [(z0 `div` 26, ws ++ [w]) | (z0, ws) <- gs, w <- [1..9], (z0 `mod` 26) - n - w == 0]