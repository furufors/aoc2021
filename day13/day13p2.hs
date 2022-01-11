#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf)
type Sheet = [[Bool]]
data Instruction = FoldAtX Int | FoldAtY Int

main = interact $ toStr . (\(i,s) -> foldl vik s i) . parsein
    where
        vik :: Sheet -> Instruction -> Sheet
        vik s (FoldAtX x) = map (\r -> zipWith (||) (take x r) (take x . reverse $ take (2 * x + 1) (r ++ repeat False))) s
        vik s (FoldAtY y) = zipWith (zipWith (||)) (take y s) (take y . reverse $ take (2 * y + 1) (s ++ repeat [False | _ <- [0..length (head s)]]))

        parsein :: String -> ([Instruction], Sheet)
        parsein = fmap toSheet . foldl parseline ([],[]) . lines

        parseline :: ([Instruction], [(Int, Int)]) -> String -> ([Instruction], [(Int, Int)])
        parseline (i, p) s = if s == [] then (i,p)
                             else if elem (head s) ['0'..'9']
                                  then let (a,b) = span (/=',') s in (i, (read a, read (tail b)):p)
                                  else if isPrefixOf "fold along y=" s
                                       then (i ++ [FoldAtY (read . drop 13 $ s)],p)
                                       else if isPrefixOf "fold along x=" s
                                            then (i ++ [FoldAtX (read . drop 13 $ s)],p)
                                            else error $ "Could not parse " ++ s

        toSheet :: [(Int, Int)] -> Sheet
        toSheet is = let mx = maximum (map fst is) 
                         my = maximum (map snd is)
                     in foldl (flip $ replace2D (const True)) [[False | _ <- [0..mx]] | _ <- [0..my]] is

        replace :: (a -> a) -> Int -> [a] -> [a]
        replace f 0 (x:xs) = (f x):xs
        replace f i (x:xs) = x : replace f (i-1) xs
        replace f i [] = []

        replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
        replace2D f (x,y) = replace (replace f x) y

        toStr :: Sheet -> String
        toStr = concat . map ((++"\n") . map (\x -> if x then '#' else '.'))
        