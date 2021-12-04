#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (reverse)
import Data.Char (digitToInt)

main = interact $ show . rate . parsein
    where
        rate l = (bitsToInt (oxygen l)) *  bitsToInt (co2scrubber l)
        genalg _     [] = error "Non conclusive"
        genalg _ (x:[]) = x
        genalg f    xs = mp : (genalg f . map tail $ filter (\x -> head x == mp) xs)
            where mp = f $ map head xs
        oxygen = genalg (\l -> if 2 * (sum l) >= length l then 1 else 0)
        co2scrubber = genalg (\l -> if 2*(sum l) >= length l then 0 else 1)
        bitsToInt l = sum [a * (2^b) | (a,b) <- zip (reverse l) [0..]]
        parsein = map (map digitToInt) . lines
