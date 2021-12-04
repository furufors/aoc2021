#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (reverse)
import Data.Char (digitToInt)

main :: IO ()
main = interact $ show . rate . parsein
    where
        rate :: [[Int]] -> Int
        rate l = (bitsToInt (oxygen l)) *  bitsToInt (co2scrubber l)
        genalg :: ([Int] -> Int) -> [[Int]] -> [Int]
        genalg _     [] = error "Non conclusive"
        genalg _ (x:[]) = x
        genalg f    xs = mp : (genalg f . map tail $ filter (\x -> head x == mp) xs)
            where mp = f $ map head xs
        oxygen, co2scrubber :: [[Int]] -> [Int]
        oxygen = genalg (\l -> if 2 * (sum l) >= length l then 1 else 0)
        co2scrubber = genalg (\l -> if 2*(sum l) >= length l then 0 else 1)
        bitsToInt :: [Int] -> Int
        bitsToInt l = sum [a * (2^b) | (a,b) <- zip (reverse l) [0..]]
        parsein :: String -> [[Int]]
        parsein = map (map digitToInt) . lines
