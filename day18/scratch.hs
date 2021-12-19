#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Maybe
import Debug.Trace (trace)
data Snailfish = Single Int | Pair (Snailfish, Snailfish)
data ScanState = ScanState {
    hasExploded :: Bool,
    doneExploding :: Bool,
    leftNumber :: Maybe Int,
    rightNumber :: Maybe Int,
    nestingDepth :: Int,
    processed :: String
}

initState :: ScanState
initState = ScanState { hasExploded = False,
                        doneExploding = False,
                        leftNumber = Nothing,
                        rightNumber = Nothing,
                        nestingDepth = 0,
                        processed = "" }

incNest :: ScanState -> ScanState
incNest s = s { nestingDepth = nestingDepth s + 1 }
decNest :: ScanState -> ScanState
decNest s = s { nestingDepth = nestingDepth s - 1 }
addChar :: Char -> ScanState -> ScanState
addChar c s = s { processed = processed s ++ [c] }
calLeftNum :: ScanState -> Int -> Int
calLeftNum st n = case leftNumber st of
    Nothing -> 0
    Just i  -> i + n
showRightNumber :: ScanState -> String
showRightNumber st = case rightNumber st of
    Nothing -> "0"
    Just i  -> show i

instance Show Snailfish where
    show (Single i) = show i
    show (Pair (a,b)) = "[" ++ show a ++ "," ++ show b ++ "]"

--main = interact $ show . magnitude . sumSnailfish . map parseSnailfish . lines
main = interact $ show . sumSnailfish. map (reduceSnailfish . parseSnailfish) . lines
    where
        parseSnailfish :: String -> Snailfish
        parseSnailfish s =
            if head s == '[' then let (a,b) = splitToPair s in Pair (parseSnailfish a, parseSnailfish b)
            else if elem (head s) ['0'..'9'] then Single (read (takeWhile (\x -> elem x ['0'..'9']) s)) else error "Parse issue"

        splitToPair :: String -> (String, String)
        splitToPair = matchBrackets 1 "" . tail

        matchBrackets :: Int -> String -> String -> (String, String)
        matchBrackets (-1) s1    s0 = error "Mismatched brackets"
        matchBrackets   n  s1 [   ] = error "Empty string with unclosed brackets"
        matchBrackets   n  s1 (s:ss) = case s of
            '[' -> matchBrackets (n+1) (s1 ++ [s]) (ss)
            ',' -> if n == 1 then (s1, init ss) else matchBrackets (n) (s1 ++ [s]) (ss)
            ']' -> matchBrackets (n-1) (s1 ++ [s]) (ss)
            ___ -> matchBrackets (n) (s1 ++ [s]) (ss)

        addSnailfish :: Snailfish -> Snailfish -> Snailfish
        addSnailfish a b = reduceSnailfish $ Pair (a, b)

        reduceSnailfish :: Snailfish -> Snailfish
        reduceSnailfish s = let (b1,s1) = explode s
                                (b2,s2) = split s
                            in if b1 then reduceSnailfish s1 else s --if b2 then reduceSnailfish s2 else s 
            where
                explode :: Snailfish -> (Bool, Snailfish)
                explode = fmap parseSnailfish . explodeString initState . show
                
                explodeString :: ScanState -> String -> (Bool, String)
                explodeString st [ ] = (hasExploded st, processed st)
                --explodeString st str = trace (processed st ++ "|" ++ str) $ if doneExploding st
                explodeString st str = if doneExploding st
                    then (True, processed st ++ str) 
                    else case nestingDepth st of
                        5 -> let (a,b) = span (/=',') str 
                                 (c,d) = span (/=']') (tail b)
                                 st' = decNest st{ processed = init (processed st) ++ show (calLeftNum st (read a)), rightNumber = Just (read c), hasExploded = True }
                             in explodeString st' (tail d)
                        x -> case head str of
                               '[' -> explodeString (addChar '[' . incNest $ st) (tail str)
                               ']' -> explodeString (addChar ']' . decNest $ st) (tail str)
                               ',' -> explodeString (addChar ',' st) (tail str)
                               ___ -> let (a,b) = span (\x -> elem x ['0'..'9']) str
                                      in if hasExploded st
                                         then explodeString (st { processed = processed st ++ showRightNumber st, doneExploding = True }) b
                                         else explodeString (st { processed = processed st ++ a, leftNumber = Just (read a)}) b



                -- explode :: Snailfish -> (Bool, Snailfish)
                -- explode = (\(a,b,c) -> (a,c)) . ex' 0 Nothing False
                -- ex' :: Int -> Maybe Int -> Bool -> Snailfish -> (Bool, Maybe Int, Snailfish)
                -- ex' _ _ True x = (True, Nothing, x)
                -- ex' _ _ b (Single i) = (b, Nothing, Single i)
                -- ex' 3 n False r@(Pair (Single i, Single j)) = (False, Nothing, r)
                -- ex' 3 n False (Pair (Pair (Single a, Single b), Single c)) = (True, Nothing, Pair (Single (rule n a), Single (b + c)))
                -- ex' 3 n False (Pair (Single c, Pair (Single a, Single b))) = (True, Just b, Pair (Single (a + c), Single 0))
                -- ex' 3 n False (Pair (_,_)) = error "Explosion with neighbouring pairs"
                -- ex' l n False (Pair (b, Single i)) = let (p, c',b') = ex' (l+1) n False b in case c' of 
                --     Just c  -> (p, Nothing, Pair (b', Single (i + c)))
                --     Nothing -> (p, Nothing, Pair (b', Single i))
                -- ex' l n False (Pair (Single i, b)) = let (p, c',b') = ex' (l+1) (Just i) False b in (p, c', Pair (Single i, b'))
                -- ex' l n False (Pair (a,b)) = let (pa,ca',a') = ex' (l+1) n False a
                --                                  (pb,cb',b') = ex' (l+1) n False b
                --                              in if pa
                --                                 then (pa, ca', Pair (a', b))
                --                                 else if pb
                --                                      then (pb, cb', Pair (a', b'))
                --                                      else (False, Nothing, Pair (a', b'))

                -- rule :: Maybe Int -> Int -> Int
                -- rule Nothing  b = 0
                -- rule (Just a) b = a + b

                split :: Snailfish -> (Bool, Snailfish)
                split s = (False, s)

        sumSnailfish :: [Snailfish] -> Snailfish
        sumSnailfish =  foldl1 addSnailfish

        magnitude :: Snailfish -> Int
        magnitude (Single x) = x
        magnitude (Pair (a,b)) = 3 * magnitude a + 2 * magnitude b