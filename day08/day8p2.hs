#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (sort)
main = interact $ show . sum . map (toDisplay . (\ws -> (takeWhile (/= "|") ws, tail $ dropWhile (/= "|") ws)) . map sort .  words) . lines
    where
        toDisplay :: ([String], [String]) -> Int
        toDisplay (s,t) = sum $ zipWith (*) (map (getInt kv) t) [1000, 100, 10, 1]
            where
                kv :: [(String,Int)]
                kv = (\m -> (findTwo   m s, 2):m) . (\m -> (findThree m s, 3):m) .
                     (\m -> (findFive  m s, 5):m) . (\m -> (findSix   m s, 6):m) .
                     (\m -> (findZero  m s, 0):m) . (\m -> (findNine  m s, 9):m) $
                     (findEight s, 8):(findSeven s, 7):(findFour s, 4):(findOne s, 1):[]
                findZero m  = head . isNot m 9 . allElemsOf m 1 . lengthIs 6
                findOne     = head . lengthIs 2
                findTwo m   = head . isNot m 5 . isNot m 3 . lengthIs 5
                findThree m = head . allElemsOf m 1 . lengthIs 5
                findFour    = head . lengthIs 4
                findFive m  = head . allElemsIn m 6 . lengthIs 5
                findSix m   = head . isNot m 9 . isNot m 0 . lengthIs 6
                findSeven   = head . lengthIs 3
                findEight   = head . lengthIs 7
                findNine m  = head . allElemsOf m 4 . lengthIs 6

        getStr :: [(String, Int)] -> Int -> String
        getStr [        ] _ = error "Logic broken"
        getStr ((s,i):ss) n = if i == n then s else getStr ss n

        getInt :: [(String, Int)] -> String -> Int
        getInt [        ] _ = error "Logic broken"
        getInt ((s,i):ss) t = if s == t then i else getInt ss t

        lengthIs :: Int -> [String] -> [String]
        lengthIs n = filter ((==n) . length)

        allElemsOf, allElemsIn :: [(String, Int)] -> Int -> [String] -> [String]
        allElemsOf m i = filter (allElemsOf' (getStr m i))
        allElemsIn m i = filter (flip allElemsOf' (getStr m i))

        allElemsOf' :: String -> String -> Bool
        allElemsOf' [] t     = True
        allElemsOf' (s:ss) t = elem s t && allElemsOf' ss t

        isNot :: [(String, Int)] -> Int -> [String] -> [String]
        isNot m i = filter (not . (== getStr m i))

-- 1 = length 2
-- 4 = length 4
-- 7 = length 3
-- 8 = length 7
-- 9 = length 6 && all elements of 4
-- 0 = not 9 && length 6 && all elements of 1
-- 6 = not 9 && not 0 && length 6
-- 5 = length 5 and all elements in 6
-- 3 = length 5 and all elements of 1
-- 2 = length 5 && not 5 && not 3

