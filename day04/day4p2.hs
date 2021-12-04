#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (transpose)
data Cell = Hit | Open Int deriving Show
type Board = [[Cell]]
type Callout = [Int]
type GameStatus = (Callout, [Board])

main :: IO ()
main = interact $ show . play . parsein
    where
        play :: GameStatus -> Int
        play ([  ],  _) = error "No winner"
        play (ns0, bs0) = let bs = gameStep (head ns0) bs0 in
                          if winner bs
                          then score (head ns0) (head bs)
                          else play (tail ns0, filter (not . solved) bs)

        winner :: [Board] -> Bool
        winner = all solved

        solved :: Board -> Bool
        solved b = let h = map (map toHit) b 
                   in row h || col h

        row = any (== 5) . map sum
        col = row . transpose

        gameStep ::  Int -> [Board] -> [Board]
        gameStep n bs = map (map (map (is (n)))) bs
        
        is :: Int -> Cell -> Cell
        is n Hit = Hit
        is n (Open i) = if n == i then Hit else Open i
        
        score :: Int -> Board -> Int
        score n x = n * getScore x
        
        getScore :: Board -> Int
        getScore = sum . map (sum . (map toI))

        toI, toHit :: Cell -> Int
        toI        Hit = 0
        toI   (Open i) = i
        toHit      Hit = 1
        toHit (Open _) = 0

        parsein :: String -> GameStatus
        parsein = p1 . map (map read . words) . filter (/= []) . lines . map repl
            where
                repl :: Char -> Char
                repl ',' = ' '
                repl   x = x
                p1 xs = (head xs, chunks 5 . map (map Open) $ tail xs)
                chunks :: Int -> [a] -> [[a]]
                chunks _ [] = []
                chunks n l
                  | n > 0 = (take n l) : (chunks n (drop n l))
                  | otherwise = error "Chunking issue"
