#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
type Node = (Int, (Int, Int))

main = interact $ show . minPathSum . parsein
    where
        minPathSum :: [[Int]] -> Int
        minPathSum s = case minPath step (m,n) (0, (0,0)) of Nothing -> 0; Just x -> fst x
            where
                m = (length . head $ s) - 1
                n = (length s) - 1
                step :: Node -> [Node]
                step (c,(x,y)) = top ++ left ++ right ++ bottom
                    where
                        top    = if y > 0 then [(c + s!!(y-1)!!(x), (x, y-1))] else []
                        left   = if x > 0 then [(c + s!!(y)!!(x-1), (x-1, y))] else []
                        right  = if x < m then [(c + s!!(y)!!(x+1), (x+1, y))] else []
                        bottom = if y < n then [(c + s!!(y+1)!!(x), (x, y+1))] else []

        minPath :: (Ord cost , Ord node) => ((cost , node) -> [(cost , node)]) -> node -> (cost , node) -> Maybe (cost , node)
        minPath step goal start = search mempty (Set.singleton start)
            where
                search visited toBeVisited = case Set.minView toBeVisited of
                    Nothing -> Nothing
                    Just ((cost , pos) , withoutPos)
                        | pos == goal              -> Just (cost , pos)
                        | pos `Set.member` visited -> search visited withoutPos
                        | otherwise                -> search visitedWithNode withNext
                        where
                            visitedWithNode = Set.insert pos visited
                            withNext = foldr Set.insert withoutPos $ step (cost , pos)

        parsein :: String -> [[Int]]
        parsein = map (map read . chunksOf 1) . lines
