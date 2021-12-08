#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main = interact $ show . length . 
                  filter (flip elem [2,3,4,7] . length) .
                  concat . map (tail . dropWhile (/= "|") . words) . lines