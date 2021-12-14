#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort)
type Polymer = String
type Rule = ((Char, Char), Char)

main = interact $ show . score . researchPolymer 10 . parsein
    where
        researchPolymer :: Int -> ([Rule], Polymer) -> Polymer
        researchPolymer n (rs,p) = (iterate (updatePolymer rs) p)!!n

        updatePolymer :: [Rule] -> Polymer -> Polymer
        updatePolymer rs p = concat (zipWith (edit rs) p (tail p)) ++ [last p]

        edit :: [Rule] -> Char -> Char -> String
        edit [        ] a b = [a]
        edit ((c,s):rs) a b = if c == (a,b) then [a, s] else edit rs a b

        score :: String -> Int
        score s = let gs = sort . map length . group . sort $ s
                      lf = head gs
                      mf = last gs
                  in mf - lf

        parsein :: String -> ([Rule], Polymer)
        parsein = foldl parseline ([],"") . lines

        parseline :: ([Rule], Polymer) -> String -> ([Rule], Polymer) 
        parseline (rs, p) s = if s == [] then (rs,p)
                              else if elem '>' s then (((s!!0,s!!1),s!!6):rs, p)
                                   else (rs, s)
