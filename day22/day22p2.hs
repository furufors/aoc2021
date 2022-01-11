#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language BangPatterns #-}
import qualified Data.Set as S
import Data.List (sort)
import Data.List.Split
import Test.HUnit
data State = On | Off deriving (Eq, Show)
type Cube = ((Integer, Integer), (Integer, Integer), (Integer, Integer))
type Rule = (Main.State, Cube)
type Reactor = S.Set Cube

main = do
    runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17]
    interact $ show . main'
    return ()
    where
        main' = countOn . foldl (updateReactor) (S.singleton ((0,0), (0,0), (0,0))) . map parsein . lines
-- Algorithm:
-- For each existing Cube, see if the new rule updates a volume which is alredy occupied.
-- Modify the already occupied Cubes to new sections of cubes which are not overlapping.
        updateReactor :: Reactor -> Rule -> Reactor 
        updateReactor reactor rule =
            let reactor' = foldl (updateCubes rule)  (S.singleton ((0,0), (0,0), (0,0))) (S.toList reactor)
            in if fst rule == On then S.insert (snd rule) reactor' else reactor'

        updateCubes :: Rule -> Reactor -> Cube -> Reactor
        updateCubes (s, cr@(xs, ys, zs)) reactor ct =
            if (cr `contains` ct)
            then reactor
            else if overlap cr ct
                 then let newCubes = removeIntersection cr ct
                      in S.union newCubes reactor
                 else S.insert ct reactor

        contains :: Cube -> Cube -> Bool
        contains a@((xlr,xhr), (ylr,yhr), (zlr,zhr)) b@((xlt,xht), (ylt,yht), (zlt,zht)) =
            xlr <= xlt && xht <= xhr &&
            ylr <= ylt && yht <= yhr &&
            zlr <= zlt && zht <= zhr && a /= b

        overlap :: Cube -> Cube -> Bool
        overlap (xsr, ysr, zsr) (xst, yst, zst) = overlap1d xsr xst && overlap1d ysr yst && overlap1d zsr zst
       
        overlap1d :: (Integer, Integer) -> (Integer, Integer) -> Bool
        overlap1d (al, ah) (bl, bh) =  not (ah-1 < bl || bh-1 < al)                          

        checkMember :: (Integer, Integer, Integer) -> Cube -> Bool
        checkMember (x, y, z) ((xl,xh), (yl,yh), (zl,zh)) =
            x >= xl && x < xh  && y >= yl && y < yh && z >= zl && z < zh

        removeIntersection :: Cube -> Cube -> S.Set Cube
        removeIntersection cr@((xlr,xhr), (ylr,yhr), (zlr,zhr)) ct@((xlt,xht), (ylt,yht), (zlt,zht)) =
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

        countOn :: Reactor -> Integer
        countOn r = sum . map (\((xl,xh), (yl,yh), (zl,zh)) -> (xh-xl) * (yh-yl) * (zh-zl)) . S.toList $ r


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
        test15 = TestCase (assertEqual "remove long"     24 (main' "on x=1..3,y=1..3,z=1..3\noff x=-100..100,y=2..2,z=2..2\n"))
        test16 = TestCase (assertEqual "remove mul long" 20 (main' "on x=1..3,y=1..3,z=1..3\noff x=-100..100,y=2..2,z=2..2\noff x=2..2,y=-100..100,z=2..2\noff x=2..2,y=2..2,z=-100..100\n"))
        test17 = TestCase (assertEqual "testcase " 590784 (main' "on x=-20..26,y=-36..17,z=-47..7\non x=-20..33,y=-21..23,z=-26..28\non x=-22..28,y=-29..23,z=-38..16\non x=-46..7,y=-6..46,z=-50..-1\non x=-49..1,y=-3..46,z=-24..28\non x=2..47,y=-22..22,z=-23..27\non x=-27..23,y=-28..26,z=-21..29\non x=-39..5,y=-6..47,z=-3..44\non x=-30..21,y=-8..43,z=-13..34\non x=-22..26,y=-27..20,z=-29..19\noff x=-48..-32,y=26..41,z=-47..-37\non x=-12..35,y=6..50,z=-50..-2\noff x=-48..-32,y=-32..-16,z=-15..-5\non x=-18..26,y=-33..15,z=-7..46\noff x=-40..-22,y=-38..-28,z=23..41\non x=-16..35,y=-41..10,z=-47..6\noff x=-32..-23,y=11..30,z=-14..3\non x=-49..-5,y=-3..45,z=-29..18\noff x=18..30,y=-20..-8,z=-3..13\non x=-41..9,y=-7..43,z=-33..15"))
