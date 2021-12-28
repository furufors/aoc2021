#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language BangPatterns #-}
import qualified Data.Set as S
import Data.List (sort, nub)
import Data.List.Split
import Debug.Trace (trace)
import Test.HUnit
data State = On | Off deriving Eq
type Cube = ((Int, Int), (Int, Int), (Int, Int))
type Rule = (Main.State, Cube)
type Reactor = S.Set Cube

main = do
    runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14]
    interact $ show . main'
    return ()
    where
        main' = countOn . containedWithin . foldl (updateReactor) (S.singleton ((0,0), (0,0), (0,0))) . map parsein . lines
-- Algorithm:
-- For each existing Cube, see if the new rule updates an volume which is alredy occupied.
-- Modify the already occupied Cubes to new sections of cubes which are not overlapping.
        updateReactor :: Reactor -> Rule -> Reactor 
        updateReactor reactor rule =
            let (bool, reactor') = foldl (updateCubes rule) (False, reactor) (S.toList reactor)
            in if not bool && fst rule == On then S.insert (snd rule) reactor' else reactor'

        updateCubes :: Rule -> (Bool, Reactor) -> Cube -> (Bool, Reactor)
        updateCubes (s, cr@(xs, ys, zs)) (bool, reactor) ct =
            if (s == On && cr `contains` ct)
            then (bool, S.delete ct reactor)
            else if (s == Off && cr `contains` ct)
                 then (bool, S.delete ct reactor)
                 else if overlap cr ct
                      then case s of
                             Off -> let newCubes = splitAndRemoveCubes cr ct
                                    in (True, S.union newCubes $ S.delete ct $ S.delete cr reactor)
                             On  -> let newCubes = splitAndAddCubes cr ct
                                    in (True, S.union newCubes $ S.delete ct $ S.delete cr reactor)
                      else (bool, reactor)

        containedWithin :: Reactor -> Reactor
        containedWithin r = foldl removeContained r (S.toList r)

        removeContained :: Reactor -> Cube -> Reactor
        removeContained r c = foldl (containedCube c) r (S.toList r)

        containedCube :: Cube -> Reactor -> Cube -> Reactor
        containedCube c r c' = if c `contains` c'  then S.delete c' r else r

        contains :: Cube -> Cube -> Bool
        contains a@((xlr,xhr), (ylr,yhr), (zlr,zhr)) b@((xlt,xht), (ylt,yht), (zlt,zht)) =
            xlr <= xlt && xht <= xhr &&
            ylr <= ylt && yht <= yhr &&
            zlr <= zlt && zht <= zhr && a /= b

        overlap :: Cube -> Cube -> Bool
        overlap (xsr, ysr, zsr) (xst, yst, zst) = overlap1d xsr xst && overlap1d ysr yst && overlap1d zsr zst
       
        overlap1d :: (Int, Int) -> (Int, Int) -> Bool
        overlap1d (al, ah) (bl, bh) =  not (ah <= bl || bh <= al)

        checkMember :: (Int, Int, Int) -> Cube -> Bool
        checkMember (x, y, z) ((xl,xh), (yl,yh), (zl,zh)) =
            x >= xl && x < xh  && y >= yl && y < yh && z >= zl && z < zh

        splitAndAddCubes :: Cube -> Cube -> S.Set Cube
        splitAndAddCubes cr@((xlr,xhr), (ylr,yhr), (zlr,zhr)) ct@((xlt,xht), (ylt,yht), (zlt,zht)) =
            let xs = sort $ [xlr, xlt, xhr, xht]
                ys = sort $ [ylr, ylt, yhr, yht]
                zs = sort $ [zlr, zlt, zhr, zht]
            in S.fromList [((xl,xh), (yl,yh), (zl,zh))
                          | (xl, xh) <- zip xs (tail xs)
                          , (yl, yh) <- zip ys (tail ys)
                          , (zl, zh) <- zip zs (tail zs)
                          , checkMember (xl, yl, zl) cr || checkMember (xl, yl, zl) ct 
                          ]

        splitAndRemoveCubes :: Cube -> Cube -> S.Set Cube
        splitAndRemoveCubes cr@((xlr,xhr), (ylr,yhr), (zlr,zhr)) ct@((xlt,xht), (ylt,yht), (zlt,zht)) =
            let xs = sort $ [xlr, xlt, xhr, xht]
                ys = sort $ [ylr, ylt, yhr, yht]
                zs = sort $ [zlr, zlt, zhr, zht]
            in S.fromList [((xl,xh), (yl,yh), (zl,zh))
                          | (xl, xh) <- zip xs (tail xs)
                          , (yl, yh) <- zip ys (tail ys)
                          , (zl, zh) <- zip zs (tail zs)
                          , not (checkMember (xl, yl, zl) cr) && checkMember (xl, yl, zl) ct
                          ]

        parsein :: String -> Rule
        parsein s =
            let (state, rest) = span (/=' ') s
                (x:y:z:_) = splitOn "," (tail rest)
                isDig e = elem e ("-"++['0'..'9'])
                fstDig = (\(x,y) -> (read x, y)) . span isDig . drop 2
                sndDig = (\(x,y) -> (read x, y)) . span isDig . drop 2
                (xl, x') = fstDig x
                (xh, _) = sndDig x'
                (yl, y') = fstDig y
                (yh, _) = sndDig y'
                (zl, z') = fstDig z
                (zh, _) = sndDig z'
            in (if state == "on" then On else Off, ((min xl xh, 1 + max xl xh), (min yl yh, 1 + max yl yh), (min zl zh, 1 + max zl zh)))

        countOn :: Reactor -> Int
        countOn r = {-trace (show r) $-} sum . map (\((xl,xh), (yl,yh), (zl,zh)) -> (xh-xl) * (yh-yl) * (zh-zl)) . S.toList $ r


        test1 = TestCase (assertEqual "default"            27 (main' "on x=1..3,y=1..3,z=1..3\n"))
        test2 = TestCase (assertEqual "addtwice"           27 (main' "on x=1..3,y=1..3,z=1..3\non x=1..3,y=1..3,z=1..3\n"))
        test3 = TestCase (assertEqual "remove from middle" 26 (main' "on x=1..3,y=1..3,z=1..3\noff x=2..2,y=2..2,z=2..2\n"))
        test4 = TestCase (assertEqual "remove same"         0 (main' "on x=1..3,y=1..3,z=1..3\noff x=1..3,y=1..3,z=1..3\n"))
        test5 = TestCase (assertEqual "remove from corner" 26 (main' "on x=1..3,y=1..3,z=1..3\noff x=3..3,y=3..3,z=3..3\n"))
        test6 = TestCase (assertEqual "add overlap"        27 (main' "on x=2..2,y=2..2,z=2..2\non x=1..3,y=1..3,z=1..3\n"))
        test7 = TestCase (assertEqual "add overlap side"   27 (main' "on x=2..2,y=2..2,z=3..3\non x=1..3,y=1..3,z=1..3\n"))
        test8 = TestCase (assertEqual "add next to"        54 (main' "on x=1..3,y=1..3,z=1..3\non x=4..6,y=1..3,z=1..3\n"))
        test9 = TestCase (assertEqual "add with overlap"   45 (main' "on x=1..3,y=1..3,z=1..3\non x=3..5,y=1..3,z=1..3\n"))
        test10 = TestCase (assertEqual "contains"           8 (main' "on x=1..1,y=1..1,z=1..1\non x=1..2,y=1..2,z=1..2\n"))
        test11 = TestCase (assertEqual "add over two"      54 (main' "on x=1..3,y=1..3,z=1..3\non x=4..6,y=1..3,z=1..3\non x=2..5,y=1..3,z=1..3\n"))
        test12 = TestCase (assertEqual "remove over two"   18 (main' "on x=1..3,y=1..3,z=1..3\non x=4..6,y=1..3,z=1..3\noff x=2..5,y=1..3,z=1..3\n"))
        test13 = let (_,a) = parsein "on x=1..3,y=1..3,z=1..3\n"
                     (_,b) = parsein "on x=3..5,y=1..3,z=1..3\n"
                in TestCase (assertEqual "test overlap" True  (overlap a b))
        test14 = let (_,a) = parsein "on x=1..3,y=1..3,z=1..3\n"
                     (_,b) = parsein "on x=4..6,y=1..3,z=1..3\n"
                in TestCase (assertEqual "test overlap" False (overlap a b))
