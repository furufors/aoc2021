#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort, sortBy)
import qualified Data.Map  as M
type Polymer = M.Map (Char, Char) (Int, Int, Bool) -- Current number, next number, zero at transaction
type Rule = ((Char, Char), Char)

main = interact $ show . score . researchPolymer 40 . parsein
    where
        researchPolymer :: Int -> ([Rule], Polymer) -> Polymer
        researchPolymer n (rs,p) = (iterate (updatePolymer rs) p)!!n

        updatePolymer :: [Rule] -> Polymer -> Polymer
        updatePolymer rs p = transaction $ foldl applyRule p rs

        applyRule :: Polymer -> Rule -> Polymer
        applyRule p ((a,b),c) = let old = (\(a,_,_) -> a) $ M.findWithDefault (0,0,True) (a,b) p
                                in if old > 0
                                   then addToNext (c,b) old . addToNext (a,c) old . zeroNext (a,b) $ p
                                   else p
            where 
                addToNext k o = M.insertWith (\(aN,bN,cN) (aO,bO,cO) -> (aO, bO + bN, cO)) k (0, o, False)
                zeroNext k = M.insertWith (\(aN,bN,cN) (aO,bO,cO)  -> (aO, bO, cN)) k (0,0, True)

        transaction :: Polymer -> Polymer
        transaction = M.map (\(a,b,c) -> if c then (b, 0, False) else (a+b, 0, False))

        score :: Polymer -> Int
        score p = let highest (c,(a,b)) = (c, max a b)
                      vals = sortBy (\a b -> compare (snd a) (snd b)) . map highest . M.toList $ foldl count M.empty (M.toList p)
                      lf = snd . head $ vals
                      mf = snd . last $ vals
                  in mf - lf

        -- Right measure is to count first and second as separate, then the higher of them is the correct result.
        -- This deals with the letters used either at the start of end of the string.
        -- Probably doesn't handle Polymer seeds with the same first and last letter...
        count :: M.Map Char (Int, Int) -> ((Char,Char), (Int,Int,Bool)) -> M.Map Char (Int, Int)
        count m ((a,b), (i,_,_)) = let f = \(f1,s1) (f2,s2) -> (f1+f2,s1+s2)
                                   in M.insertWith f b (0,i) . M.insertWith f a (i,0) $ m

        parsein :: String -> ([Rule], Polymer)
        parsein = foldl parseline ([], M.empty) . lines

        parseline :: ([Rule], Polymer) -> String -> ([Rule], Polymer) 
        parseline (rs, p) s = if s == [] then (rs,p)
                              else if elem '>' s then (((s!!0,s!!1),s!!6):rs, p)
                                   else (rs, foldl addPairs M.empty (zip s (tail s)))

        addPairs :: Polymer -> (Char, Char) -> Polymer
        addPairs p c = M.insertWith (\(aN,bN,cN) (aO,bO,cO)  -> (aN+aO, 0, False)) c (1,0,False) p
