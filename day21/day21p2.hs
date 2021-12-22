#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language BangPatterns #-}
data Player = Player1 | Player2 deriving Eq
type Order = [Player]
type Score = Int
type Pos = Int
type GameStatus = ((Score, Pos), (Score, Pos))

main = putStrLn $ show . score $ game order startState
    where
        game :: Order -> GameStatus -> (Int,Int)
        game (o:os) ((s1, p1), (s2, p2)) =
            if s1 >= 21
                then (1,0)
                else if s2 >= 21
                     then (0,1)
                     else foldl1 sumpar [step 3 1, step 4 3, step 5 6, step 6 7, step 7 6, step 8 3, step 9 1]
            where
                step delta count = case o of
                    Player1 -> let p1' = p1 + delta
                                   s1' = s1 + pos2score p1'
                               in count `times` (game os ((s1', p1'), (s2, p2)))
                    Player2 -> let p2' = p2 + delta
                                   s2' = s2 + pos2score p2'
                               in count `times` (game os ((s1, p1), (s2', p2')))
                -- Player1 -> let co = [((s1', p1'), (s2, p2))
                --                     | d1 <- [1..3]
                --                     , d2 <- [1..3]
                --                     , d3 <- [1..3]
                --                     , let p1' = p1 + d1 + d2 + d3
                --                     , let s1' = s1 + pos2score p1'
                --                     , s1' < 21
                --                     ]
                --                en = [ ()
                --                     | d1 <- [1..3]
                --                     , d2 <- [1..3]
                --                     , d3 <- [1..3]
                --                     , let p1' = p1 + d1 + d2 + d3
                --                     , let s1' = s1 + pos2score p1'
                --                     , s1' > 21
                --                     ]
                --            in foldl1 sumpar ((length en, 0):co)
                -- Player2 -> let co = [ game os ((s1, p1), (s2', p2'))
                --                     | d1 <- [1..3]
                --                     , d2 <- [1..3]
                --                     , d3 <- [1..3]
                --                     , let p2' = p2 + d1 + d2 + d3
                --                     , let s2' = s2 + pos2score p2'
                --                     , s2' < 21
                --                     ]
                --                en = [ ()
                --                     | d1 <- [1..3]
                --                     , d2 <- [1..3]
                --                     , d3 <- [1..3]
                --                     , let p2' = p2 + d1 + d2 + d3
                --                     , let s2' = s2 + pos2score p2'
                --                     , s2' > 21
                --                     ]
                --            in foldl1 sumpar ((0, length en):co)
        times :: Int -> (Int, Int) -> (Int, Int)
        times n (a,b) = (n*a, n*b) 

        sumpar :: (Int, Int) -> (Int, Int) -> (Int, Int)
        sumpar (!a1, !b1) (!a2, !b2) = (a1 + a2, b1 + b2)

        score :: (Int, Int) -> Int
        score (p1, p2) = max p1 p2

        order :: Order
        order = cycle ([Player1, Player2])

        startState :: GameStatus
        startState = ((0, 8), (0, 4))

        pos2score :: Int -> Int
        pos2score i = board!!(i-1)

        board :: [Int]
        board = cycle [1..10]
