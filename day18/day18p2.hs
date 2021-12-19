#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Maybe
import Debug.Trace (trace)
data Snailfish = S Int Int | P (Snailfish, Snailfish)
data ScanState = ScanState {
    hasExploded :: Bool,
    leftNumber :: (Int, Int),
    rightNumber :: (Int, Int),
    nestingDepth :: Int
}

initState :: ScanState
initState = ScanState {
    hasExploded = False,
    leftNumber = (0,0),
    rightNumber = (0,0),
    nestingDepth = 0
}

incNest :: ScanState -> ScanState
incNest s = s { nestingDepth = nestingDepth s + 1 }
decNest :: ScanState -> ScanState
decNest s = s { nestingDepth = nestingDepth s - 1 }

instance Show Snailfish where
    show (S i v) = show v
    show (P (a,b)) = "[" ++ show a ++ "," ++ show b ++ "]"

main = interact $ show . maximum . combinations . map (reduceSnailfish . snd . parseSnailfish 0) . lines
    where
        combinations :: [Snailfish] -> [Int]
        combinations ss = let is = [0..(length ss - 1)] in [magnitude (addSnailfish (ss!!a) (ss!!b)) | a <- is, b <- is, a /= b]
        parseSnailfish :: Int -> String -> (Int, Snailfish)
        parseSnailfish n s =
            if head s == '['
            then let (a,b) = splitToP s
                     (n1, s1) = parseSnailfish n a
                     (n2, s2) = parseSnailfish n1 b
                 in (n2, P (s1, s2))
            else if elem (head s) ['0'..'9']
                 then let n1 = n + 1
                          val = read (takeWhile (\x -> elem x ['0'..'9']) s)
                      in (n1, S n val)
                 else error "Parse issue"

        splitToP :: String -> (String, String)
        splitToP = matchBrackets 1 "" . tail

        matchBrackets :: Int -> String -> String -> (String, String)
        matchBrackets (-1) s1    s0 = error "Mismatched brackets"
        matchBrackets   n  s1 [   ] = error "Empty string with unclosed brackets"
        matchBrackets   n  s1 (s:ss) = case s of
            '[' -> matchBrackets (n+1) (s1 ++ [s]) (ss)
            ',' -> if n == 1 then (s1, init ss) else matchBrackets (n) (s1 ++ [s]) (ss)
            ']' -> matchBrackets (n-1) (s1 ++ [s]) (ss)
            ___ -> matchBrackets (n) (s1 ++ [s]) (ss)

        addSnailfish :: Snailfish -> Snailfish -> Snailfish
        addSnailfish a b = reduceSnailfish $ P (a, b)

        reduceSnailfish :: Snailfish -> Snailfish
        reduceSnailfish s = let (b1,s1) = explode s
                                (b2,s2) = split s
                            in if b1 then reduceSnailfish s1 else if b2 then reduceSnailfish s2 else s 
            where
                explode :: Snailfish -> (Bool, Snailfish)
                explode = (\(a,b) -> (hasExploded a, snd (renumber 0 b))) . replaceRight . replaceLeft . ex' initState . snd . renumber 0
                    where
                        ex' :: ScanState -> Snailfish -> (ScanState, Snailfish)
                        ex' st s | hasExploded st = (st, s)
                        ex' st (P (S i a, S j b)) | nestingDepth st == 4 =
                            (st { hasExploded = True, leftNumber = (i-1,a), rightNumber = (j+1,b)}, S j 0)
                        ex' st (P (a, b))         = let (sta, a') = ex' (incNest st) a
                                                        (stb, b') = ex' sta b
                                                    in  (decNest stb, P (a',b'))
                        ex' st (S i v)            = (st, S i v)

                split :: Snailfish -> (Bool, Snailfish)
                split s = let (b,s') = sp' False s in (b, snd $ renumber 0 s')
                    where
                        sp' :: Bool -> Snailfish -> (Bool, Snailfish)
                        sp' True  s         = (True, s)
                        sp' False (S i v)   = if v > 9
                                              then let a = div v 2
                                                       b = div v 2 + mod v 2 
                                                   in (True, P (S i a, S i b))
                                              else (False, S i v)
                        sp' False (P (a,b)) = let (p1, s1) = sp' False a
                                                  (p2, s2) = sp' p1 b
                                              in (p2, P (s1, s2))

                replaceLeft :: (ScanState, Snailfish) -> (ScanState, Snailfish)
                replaceLeft (st, s) = (st, replaceVal (leftNumber st) s)
                replaceRight :: (ScanState, Snailfish) -> (ScanState, Snailfish)
                replaceRight (st, s) = (st, replaceVal (rightNumber st) s)
                replaceVal :: (Int, Int) -> Snailfish -> Snailfish
                replaceVal (i,v) (P (a,b)) = P (replaceVal (i,v) a, replaceVal (i,v) b)
                replaceVal (i,v) (S j w)   = if i == j then S i (v + w) else S j w

                renumber :: Int -> Snailfish -> (Int, Snailfish)
                renumber n (P (a, b)) = let (n1, a') = renumber n a
                                            (n2, b') = renumber n1 b
                                        in (n2, P (a',b'))
                renumber n (S _ v)    = (n+1, S n v)

        sumSnailfish :: [Snailfish] -> Snailfish
        sumSnailfish =  foldl1 addSnailfish

        magnitude :: Snailfish -> Int
        magnitude (S i v) = v
        magnitude (P (a,b)) = 3 * magnitude a + 2 * magnitude b
