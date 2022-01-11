#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (reverse, sort)
import Data.List.Split (chunksOf)
type Pos = (Int, Int)

main = interact $ show . foldl1 (*) . take 3 . reverse . sort . findBasins . pad . parsein
    where
        parsein :: String -> (Pos, [[Int]])
        parsein = (\x -> ((length x, length (head x)),x)) . map (map read . chunksOf 1) . lines
        
        pad :: (Pos, [[Int]]) -> (Pos, [[Int]])
        pad ((h,w),m) = let hrz = [take (w+2) (repeat 9)] in ((h,w), hrz ++ map (\x -> [9] ++ x ++ [9]) m ++ hrz) 

        findBasins :: (Pos, [[Int]]) -> [Int]
        findBasins ((h,w),m) = snd $ foldl findBasins2 ([],[]) [(x+1,y+1) | x <- [0..w-1], y <- [0..h-1]]
            where
                findBasins2 :: ([Pos], [Int]) -> Pos -> ([Pos], [Int])
                findBasins2 (ls, acc) (x, y) = let (ls', i) = explore ls (x, y) in (ls',i:acc)

                explore :: [Pos] -> Pos -> ([Pos], Int)
                explore ls (x, y) =
                    if m!!y!!x == 9 || elem (x, y) ls
                    then ((x, y):ls, 0)
                    else let (ls1,s1) = explore ((x, y):ls) (x-1, y)
                         in let (ls2, s2) = explore ls1 (x+1, y)
                            in let (ls3, s3) = explore ls2 (x, y-1)
                               in let (ls4, s4) = explore ls3 (x, y+1) 
                                  in ((x, y):ls4, 1 + s1 + s2 + s3 + s4)

-- Algorithm:
-- Sweep all points.
-- For each point:
--   If the point is not in the blocklist:
--   run a search starting at that point to find basin size
--   add each point swept to the block-list
--   in the end sum up size
-- Sort the resulting sizes and find top 3