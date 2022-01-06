#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# Language BangPatterns #-}
import qualified Data.Map as M
import Data.List (sort)
import Data.Either
import Data.List.Split (chunksOf)
type CharInt = Either Char Integer
data Instruction = Inp Char | Add Char CharInt | Mul Char CharInt | Div Char CharInt | Mod Char CharInt | Eql Char CharInt deriving Show
type Prog = [Instruction]
data Registers = Reg Integer Integer Integer Integer
type Prog2 = [Inst2]
data Inst2 = Choice Integer | Given Integer

main :: IO ()
main = putStrLn . show . solve2 $ task
    where

    -- Choice n: zr = 26 * z0 + w + n
    -- Given  n: zr = (z0 div 26) when z0 mod 26 - n == w
    task = [ Choice  6
           , Choice  6
           , Choice  3
           , Given  11
           , Choice  9
           , Given   1
           , Choice 13
           , Choice  6
           , Given   0
           , Choice 10
           , Given   5
           , Given  16
           , Given   7
           , Given  11
           ]

    solve :: [(Integer, [Integer])] -> [Prog] -> [Integer]
    solve !sls [    ] = map (sum . zipWith (\a b -> b * 10^a) [0..] . reverse) $ map snd $ filter ((==0) . fst) sls
    solve !sls (p:ps) = solve [(z, n:(snd sl)) | n <- [1..9], sl <- sls, let z = validMN (Reg 0 0 0 (fst sl)) p [n]] ps

    solve2 :: Prog2 -> Integer
    solve2 p = last . sort . map (sum . zipWith (\a b -> 10^a * b) [13,12..0] . snd) . filter ((==0) . fst) $ step p [(0,[])]

    step :: Prog2 -> [(Integer, [Integer])] -> [(Integer, [Integer])]
    step [             ] gs = gs
    step ((Choice n):ps) gs = step ps [(26*z0 + n + w, ws ++ [w]) | (z0, ws) <- gs, w <- [1..9]]
    step ((Given  n):ps) gs = step ps [(z0 `div` 26, ws ++ [w]) | (z0, ws) <- gs, w <- [1..9], (z0 `mod` 26) - n - w == 0]

    startReg :: Registers
    startReg = Reg 0 0 0 0

    numbers :: [[Integer]]
    numbers = map (map read . chunksOf 1 . show) . filter (not . elem '0' . show) $ [99999999999999,99999999999998..0]

    validMN :: Registers -> Prog -> [Integer] -> Integer
    validMN (Reg _ _ _ z) [] _ = z
    validMN reg ((Inp c):ops) (i:is) = let reg' = ins c i reg
                                       in validMN reg' ops is
    validMN reg ((Add a b):ops) is = let a' = lu reg a
                                         b' = if isLeft b then lu reg (fl b) else fr b
                                         reg' = ins a (a' + b') reg
                                     in validMN reg' ops is
    validMN reg ((Mul a b):ops) is = let a' = lu reg a
                                         b' = if isLeft b then lu reg (fl b) else fr b
                                         reg' = ins a (a' * b') reg
                                     in validMN reg' ops is
    validMN reg ((Div a b):ops) is = let a' = lu reg a
                                         b' = if isLeft b then lu reg (fl b) else fr b
                                         reg' = ins a (div a' b') reg
                                     in validMN reg' ops is
    validMN reg ((Mod a b):ops) is = let a' = lu reg a
                                         b' = if isLeft b then lu reg (fl b) else fr b
                                         reg' = ins a (mod a' b') reg
                                     in validMN reg' ops is
    validMN reg ((Eql a b):ops) is = let a' = lu reg a
                                         b' = if isLeft b then lu reg (fl b) else fr b
                                         reg' = ins a (if a' == b' then 1 else 0) reg
                                     in validMN reg' ops is

    fr :: CharInt -> Integer
    fr = fromRight (error "Missing right value") 

    fl :: CharInt -> Char
    fl = fromLeft (error "Missing left value") 

    lu :: Registers -> Char -> Integer
    lu (Reg w x y z) c = case c of
        'w' -> w
        'x' -> x
        'y' -> y
        'z' -> z
        otherwise -> error "lookup none-existing register"

    ins :: Char -> Integer -> Registers -> Registers
    ins c i (Reg w x y z) = case c of
        'w' -> Reg i x y z
        'x' -> Reg w i y z
        'y' -> Reg w x i z
        'z' -> Reg w x y i
        otherwise -> error "insert non-existing register"

    isDig :: String -> Bool
    isDig re = elem (head re) (['-'] ++ ['0'..'9'])

    toCharInt :: String -> CharInt
    toCharInt re = if isDig re then Right (read re) else Left (head re)

    parsein :: String -> Instruction
    parsein s = case take 3 s of
        "inp" -> Inp (head $ drop 4 s)
        "add" -> Add (head $ drop 4 s) (toCharInt (drop 6 s))
        "mul" -> Mul (head $ drop 4 s) (toCharInt (drop 6 s))
        "div" -> Div (head $ drop 4 s) (toCharInt (drop 6 s))
        "mod" -> Mod (head $ drop 4 s) (toCharInt (drop 6 s))
        "eql" -> Eql (head $ drop 4 s) (toCharInt (drop 6 s))
