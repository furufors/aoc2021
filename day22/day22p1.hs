#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language BangPatterns #-}
import qualified Data.Set as S
import Data.List.Split
data State = On | Off deriving Eq
type Rule = (State, [Int], [Int], [Int])
type Xyz = (Int, Int, Int)
type Reactor = S.Set Xyz

main = interact $ show . countOn . foldl updateReactor S.empty . map parsein . lines
    where
        updateReactor :: Reactor -> Rule -> Reactor 
        updateReactor !reactor (s, xs, ys, zs) =
            let action = if s == On then S.insert else S.delete in
            foldl (\a f -> f a) reactor [action (x,y,z) | x <- xs, y <- ys, z <- zs] 

        parsein :: String -> Rule
        parsein s =
            let (state, rest) = span (/=' ') s
                (x:y:z:_) = splitOn "," (tail rest)
                isDig e = elem e ("-"++['0'..'9'])
                fstDig = (\(x,y) -> (read x, y)) . span isDig . drop 2
                sndDig = (\(x,y) -> (read x, y)) . span isDig . drop 2
                (xl, x') = fstDig x
                (xh, _) = sndDig x'
                (yl, y') = fstDig y
                (yh, _) = sndDig y'
                (zl, z') = fstDig z
                (zh, _) = sndDig z'
            in (if state == "on" then On else Off, guard [xl..xh], guard [yl..yh], guard [zl..zh])

        guard :: [Int] -> [Int]
        guard = filter (\e -> elem e [-50..50])

        countOn :: Reactor -> Int
        countOn = S.size