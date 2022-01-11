#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (transpose, reverse)
import Data.Char (digitToInt)

main :: IO ()
main = interact $ show . multiply . γε . parsein
    where
        multiply :: (Int, Int) -> Int
        multiply (x,y) = x * y
        γε :: [[Int]] -> (Int, Int)
        γε = (\x -> (bitsToInt x, bitsToInt (compl x))) . map mostPrevalent
        mostPrevalent :: [Int] -> Int
        mostPrevalent l = if 2 * (sum l) >= length l then 1 else 0
        bitsToInt :: [Int] -> Int
        bitsToInt l = sum [a * (2^b) | (a,b) <- zip (reverse l) [0..]]
        compl :: [Int] -> [Int]
        compl = map (\x -> if x == 0 then 1 else 0)
        parsein :: String -> [[Int]]
        parsein = transpose . map (map digitToInt) . lines
