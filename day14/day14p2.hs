#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort)
import qualified Data.Map  as M
type Polymer = M.Map (Char, Char) (Int, Int, Bool) -- Current number, next number, updated
type Rule = ((Char, Char), Char)

main = interact $ show . score . researchPolymer 2 . parsein
    where
        researchPolymer :: Int -> ([Rule], Polymer) -> Polymer
        researchPolymer n (rs,p) = (iterate (updatePolymer rs) p)!!n

        updatePolymer :: [Rule] -> Polymer -> Polymer
        updatePolymer rs p = transaction $ foldl applyRule p rs

        applyRule :: Polymer -> Rule -> Polymer
        applyRule p ((a,b),c) = let old = (\(a,_,_) -> a) $ M.findWithDefault (0,0,True) (a,b) p
                                in addToNext (c,b) old . addToNext (a,c) old . zeroNext (a,b) $ p
            where 
                addToNext k o = M.insertWith (\(_,b,_) (c,d,_) -> (c, b+d, True)) k (0, o, True)
                zeroNext k = M.insertWith (\_ (a,_,_) -> (a, 0, True)) k (0,0, True)

        transaction :: Polymer -> Polymer
        transaction = M.map (\(a,b,c) -> if c then (b, 0, False) else (a, 0, False))

        score :: Polymer -> Polymer
        score p = p

        parsein :: String -> ([Rule], Polymer)
        parsein = foldl parseline ([], M.empty) . lines

        parseline :: ([Rule], Polymer) -> String -> ([Rule], Polymer) 
        parseline (rs, p) s = if s == [] then (rs,p)
                              else if elem '>' s then (((s!!0,s!!1),s!!6):rs, p)
                                   else (rs, foldl addPairs M.empty (zip s (tail s)))

        addPairs :: Polymer -> (Char, Char) -> Polymer
        addPairs p c = M.insertWith (\(a,_,_) (c,_,_) -> (a+c, 0, False)) c (1,0,False) p
