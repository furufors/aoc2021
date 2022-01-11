#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Either
import Data.List.Split (chunksOf)
type CharInt = Either Char Int
data Instruction = Inp Char | Add Char CharInt | Mul Char CharInt | Div Char CharInt | Mod Char CharInt | Eql Char CharInt deriving Show
type Prog = [Instruction]
--type Registers = M.Map Char Int
data Registers = Reg Int Int Int Int

main :: IO ()
main = interact $ show . solve [(0, [])] . chunksOf 18 . map parsein . lines
    where

    solve :: [(Int, [Int])] -> [Prog] -> [Int]
    solve sls [    ] = map (sum . zipWith (\a b -> b * 10^a) [0..] . reverse) $ map snd $ filter ((==0) . fst) sls
    solve sls (p:ps) = solve [(z, n:(snd sl)) | n <- [1..9], sl <- sls, let z = validMN (Reg 0 0 0 (fst sl)) p [n]] ps

    startReg :: Registers
    startReg = Reg 0 0 0 0
    --startReg = M.insert 'w' 0 $ M.insert 'x' 0 $ M.insert 'y' 0 $ M.insert 'z' 0 $ M.empty

    numbers :: [[Int]]
    numbers = map (map read . chunksOf 1 . show) . filter (not . elem '0' . show) $ [11111111111111..]-- [99999999999999,99999999999998..0]

    validMN :: Registers -> Prog -> [Int] -> Int
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

    fr :: CharInt -> Int
    fr = fromRight (error "Missing right value") 

    fl :: CharInt -> Char
    fl = fromLeft (error "Missing left value") 

    lu :: Registers -> Char -> Int
    lu (Reg w x y z) c = case c of
        'w' -> w
        'x' -> x
        'y' -> y
        'z' -> z
        otherwise -> error "lookup none-existing register"
    --lu reg c = case M.lookup c reg of
    --             Nothing -> error "Empty reg"
    --             Just i -> i

    ins :: Char -> Int -> Registers -> Registers
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
