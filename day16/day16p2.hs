#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Debug.Trace (trace)
import Data.List (reverse)
import Data.List.Split (chunksOf)
type BinString = String
type Version = Int
type Value = Int
data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving Show
data Package = Literal Version Value | Operator Op Version [Package] deriving Show

main = interact $ show .  map (map evaluate . parsePackages . hexstringToB4) . lines

evaluate :: Package -> Int
evaluate (Literal _ v) = v
evaluate (Operator Sum _ ps )= sum . map evaluate $ ps
evaluate (Operator Product _ ps) = product . map evaluate $ ps
evaluate (Operator Minimum _ ps) = minimum . map evaluate $ ps
evaluate (Operator Maximum _ ps) = maximum . map evaluate $ ps
evaluate (Operator GreaterThan _ ps) = let (a:b:_) = map evaluate ps in if a > b then 1 else 0
evaluate (Operator LessThan _ ps) = let (a:b:_) = map evaluate ps in if a < b then 1 else 0 
evaluate (Operator EqualTo _ ps) = let (a:b:_) = map evaluate ps in if a == b then 1 else 0

parsePackages :: BinString -> [Package]
parsePackages [] = []
parsePackages s  =
    if all (=='0') s
    then []
    else let typ = binToInt . drop 3 . take 6 $ s 
             ver = binToInt . take 3 $ s
             rest = drop 6 s
         in case typ of
            0         -> parseOperator Sum ver rest
            1         -> parseOperator Product ver rest
            2         -> parseOperator Minimum ver rest
            3         -> parseOperator Maximum ver rest
            4         -> parseLiteral ver rest
            5         -> parseOperator GreaterThan ver rest
            6         -> parseOperator LessThan ver rest
            7         -> parseOperator EqualTo ver rest
            otherwise -> error "Unkown Operator package type"

parseLiteral :: Version -> BinString -> [Package]
parseLiteral ver s = let (val,rest) = parseLiteral' s in (Literal ver (binToInt val)):parsePackages rest

parseLiteral' :: BinString -> (BinString, BinString)
parseLiteral' s | length s < 5 = error "BinString ended while parsing Literal"
parseLiteral' (a:b:c:d:e:bs) = if a == '1' then (\(v,bs') -> ([b,c,d,e] ++ v, bs')) (parseLiteral' bs) else ([b,c,d,e], bs)

parseOperator :: Op -> Version -> BinString -> [Package]
parseOperator op ver []     = error "BinString ended while parsing Operator"
parseOperator op ver (s:bs) = 
    if s == '0'
    then let bits = binToInt . take 15 $ bs
             subPacketData = take bits . drop 15 $ bs 
             rest = drop (15 + bits) $ bs
         in  (Operator op ver (parsePackages subPacketData)):(parsePackages rest)
    else let packageCount = binToInt . take 11 $ bs
             remainingPackages = parsePackages . drop 11 $ bs
         in  (Operator op ver (take packageCount remainingPackages)):(drop packageCount remainingPackages)

hexstringToB4 :: String -> BinString
hexstringToB4 = concat . map toB4

binToInt :: BinString -> Int
binToInt = sum . zipWith (\e b -> (read b) * (2^e)) [0..] . chunksOf 1 . reverse

toB4 :: Char -> String
toB4 '0' = "0000"
toB4 '1' = "0001"
toB4 '2' = "0010"
toB4 '3' = "0011"
toB4 '4' = "0100"
toB4 '5' = "0101"
toB4 '6' = "0110"
toB4 '7' = "0111"
toB4 '8' = "1000"
toB4 '9' = "1001"
toB4 'A' = "1010"
toB4 'B' = "1011"
toB4 'C' = "1100"
toB4 'D' = "1101"
toB4 'E' = "1110"
toB4 'F' = "1111"
toB4 ___ = error "Non hexadecimal input given!"
