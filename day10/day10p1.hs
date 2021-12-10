#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (chunksOf)

main = interact $ show . sum . map (score . check []) . lines
    where
        check :: [Char] -> String -> Char
        check [    ] (t:tt) = check [t] tt
        check ______ [    ] = '0'
        check (s:ss) (t:tt) = if elem t ['(', '[', '{', '<']
                              then check (t:s:ss) tt
                              else if match s t
                                   then check ss tt
                                   else t
        
        match :: Char -> Char -> Bool
        match '(' ')' = True
        match '[' ']' = True
        match '{' '}' = True
        match '<' '>' = True
        match _x_ _y_ = False
        
        score :: Char -> Int
        score ')' = 3
        score ']' = 57
        score '}' = 1197
        score '>' = 25137
        score _x_ = 0
