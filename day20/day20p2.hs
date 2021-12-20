#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
import Data.List (intercalate)
import Debug.Trace (trace)
data Pixel = Light | Dark deriving Eq
type Image = [[Pixel]]
type ImEnAlg = [Pixel]

instance Show Pixel where
    show Light = "."
    show Dark  = "#"

main = interact $ show . (\i -> trace (paint i) $ darkCount i) . (\(alg,img) -> enhance 50 alg img) . parsein . lines
    where
        parsein :: [String] -> (ImEnAlg, Image)
        parsein [    ] = error "parsein: Empty input" 
        parsein (s:ss) = (toPixels s, padN 2 Light $ map toPixels (filter (not . null) ss))

        padN :: Int -> Pixel -> Image -> Image
        padN n p img = (iterate (pad p) img)!!n

        pad :: Pixel -> Image -> Image
        pad p ss = let width = length (head ss)
                       horiz = take (width+2) $ repeat p
                       sides = \s -> [p] ++ s ++ [p]
                   in [horiz] ++ map sides ss ++ [horiz]

        enhance :: Int -> ImEnAlg -> Image -> Image
        enhance 0 alg img = img
        enhance n alg img =
            let width  = length (head img)
                height = length img
                next = [[resolve alg (sample img x y) | x <- [1..(width-2)]] | y <- [1..(height-2)]]
            in enhance (n-1) alg $ padN 2 (next!!0!!0) next 

        sample :: Image -> Int -> Int -> [Pixel]
        sample img x y = [img!!y'!!x' | y' <- [y-1, y, y+1], x' <- [x-1, x, x+1]]

        resolve :: ImEnAlg -> [Pixel] -> Pixel
        resolve alg sample = alg!!(sampleToIndex sample)

        toPixels :: String -> [Pixel]
        toPixels [    ] = []
        toPixels (s:ss) = case s of
            '.' -> Light:(toPixels ss)
            '#' -> Dark:(toPixels ss)
            ___ -> error "toPixels: non-pixel content (./#)"

        sampleToIndex :: [Pixel] -> Int
        sampleToIndex = sum . map (\(i, p) -> if p == Dark then 2^i else 0) . zip [0..] . reverse

        darkCount :: Image -> Int
        darkCount = sum . map (\case Dark -> 1; Light -> 0) . concat

        paint :: Image -> String
        paint = intercalate "\n" . map (concat . map show)
