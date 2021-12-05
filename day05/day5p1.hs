#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
import qualified Data.Map as Map
type RawLine = ((Int, Int), (Int, Int)) -- ((x1, y1), (x2, y2))
data Line = H RawLine
          | V RawLine
          | D RawLine deriving Show
type Vent = Map.Map (Int, Int) Int

main :: IO ()
main = interact $ show . score . play . filter (isHV) . map hvd . parsein
    where
        play :: [Line] -> Vent
        play ls = foldl step Map.empty ls
            where
                extent :: [Line] -> Int
                extent                        [] = 1
                extent ((H ((a, b), (c, d))):xs) = maximum $ [a,b,c,d,extent xs]
                extent ((V ((a, b), (c, d))):xs) = maximum $ [a,b,c,d,extent xs]
                extent ((D ((a, b), (c, d))):xs) = maximum $ [a,b,c,d,extent xs]
                size :: Int
                size = extent ls
                step :: Vent -> Line -> Vent
                step m (H ((a,b),(_,d))) = foldl updateElem m $ zip (repeat  a) (range b d)
                step m (V ((a,b),(c,_))) = foldl updateElem m $ zip (range a c) (repeat  b)
                step m (D ((a,b),(c,d))) = foldl updateElem m $ zip (range a c) (range b d)
                range :: Int -> Int -> [Int]
                range x y = if x < y then [x..y] else reverse [y..x]
                updateElem :: Vent -> (Int,Int) -> Vent
                updateElem m k = Map.insert k ((Map.findWithDefault 0 k m) + 1) m

        score :: Vent -> Int
        score = length . filter (>1) . Map.elems

        isHV :: Line -> Bool
        isHV (H _) = True
        isHV (V _) = True
        isHV _____ = False

        hvd :: RawLine -> Line
        hvd = \case
            l@((a, _), (c, _)) | a == c -> H l
            l@((_, b), (_, d)) | b == d -> V l
            l                           -> D l

        parsein :: String -> [RawLine]
        parsein = map (\x -> ((x!!0, x!!1), (x!!2, x!!3))) . map (map read . words) . lines . map repl
                  where
                    repl ',' = ' '
                    repl '-' = ' '
                    repl '>' = ' '
                    repl  x  =  x
