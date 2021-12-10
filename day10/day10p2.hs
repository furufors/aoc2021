#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (reverse, sort)
import Data.List.Split (chunksOf)

main = interact $ show . middle . sort . map (score . complete []) . filter (not . malformed []) . lines
    where
        malformed :: [Char] -> String -> Bool
        malformed [    ] (t:tt) = malformed [t] tt
        malformed ______ [    ] = False
        malformed (s:ss) (t:tt) = if elem t ['(', '[', '{', '<']
                                  then malformed (t:s:ss) tt
                                  else if match s t
                                       then malformed ss tt
                                       else True
        
        complete :: String -> [Char] -> [Char]
        complete [    ] (t:tt) = complete [t] tt
        complete s      [    ] = map matching s
        complete (s:ss) (t:tt) = if elem t ['(', '[', '{', '<']
                                 then complete (t:s:ss) tt
                                 else if match s t
                                      then complete ss tt
                                      else error "Cannot complete malformed expression"

        matching :: Char -> Char
        matching '(' = ')'
        matching '[' = ']'
        matching '{' = '}'
        matching '<' = '>'
        matching ___ = error "Malformed input"

        match :: Char -> Char -> Bool
        match c d = d == matching c
        
        score:: [Char] -> Int
        score = foldl (\s c -> 5*s + score' c) 0

        score' :: Char -> Int
        score' ')' = 1
        score' ']' = 2
        score' '}' = 3
        score' '>' = 4
        score' _x_ = 0

        middle :: [a] -> a
        middle ss = let i = div (length ss) 2 in ss!!i
