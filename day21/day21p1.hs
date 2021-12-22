#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language LambdaCase #-}
data Player = Player1 | Player2
type Order = [Player]
type Die = [Int]
type Score = Int
type Pos = Int
type GameStatus = ((Score, Pos), (Score, Pos), Int, Die)

main = putStrLn $ show . score . head . dropWhile (not . thousand) $ game order startState
    where
        game :: Order -> GameStatus -> [GameStatus]
        game (o:os) oldState@((s1, p1), (s2, p2), i, d) =
            case o of
                Player1 -> let (is, d') = roll d
                               p1' = p1 + sum is
                               s1' = s1 + pos2score p1'
                               newState = ((s1', p1'), (s2, p2), i+3, d')
                           in (oldState):(game os newState)
                Player2 -> let (is, d') = roll d
                               p2' = p2 + sum is
                               s2' = s2 + pos2score p2'
                               newState = ((s1, p1), (s2', p2'), i+3, d')
                           in (oldState):(game os newState)                           

        score :: GameStatus -> Int
        score ((s1, _), (s2, _), i, _) = if s1 < s2 then i * s1 else i * s2

        order :: Order
        order = cycle ([Player1, Player2])

        startState :: GameStatus
        startState = ((0, 8), (0, 4), 0, newDie)

        thousand :: GameStatus -> Bool
        thousand ((s1, _), (s2, _), _, _) = s1 >= 1000 || s2 >= 1000

        pos2score :: Int -> Int
        pos2score i = board!!(i-1)

        board :: [Int]
        board = cycle [1..10]

        newDie :: [Int]
        newDie = cycle [1..100]

        roll :: Die -> ([Int], Die)
        roll d = (take 3 d, drop 3 d)
