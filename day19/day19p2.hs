#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Set as S
import Data.List (isPrefixOf, reverse)
import Data.List.Split (splitWhen)
import Debug.Trace (trace)
type Xyz = (Int, Int, Int)
type ScannerData = [Xyz]
type Environment = S.Set Xyz

main = interact $ show . manhattanDistance . reconstruct . parsein . lines
    where
        reconstruct :: [ScannerData] -> [Xyz]
        reconstruct (s1:ss) = let m = addScannerDataToEnv S.empty s1 in reconstructScanner m [] ss [(0,0,0)]

        reconstructScanner :: Environment -> [ScannerData] -> [ScannerData] -> [Xyz] -> [Xyz]
        reconstructScanner e [] [    ] ps = ps
        reconstructScanner e (b:bs) [] ps = reconstructScanner (S.fromList b) [] ((S.toList e):bs) ps
        reconstructScanner e bs (s:ss) ps = trace (show (length ss + length bs + 1)) $
                                            let res = align e s
                                            in case res of
                                                [    ] -> reconstructScanner e (s:bs) ss ps
                                                (a:aa) -> let e' = S.union e (S.fromList (fst a))
                                                          in reconstructScanner e' [] (bs++ss) ((snd a):ps)

        align :: Environment -> ScannerData -> [(ScannerData, Xyz)]
        align e s = let toMinMax = \a -> [(S.findMin a - 1000)..(S.findMax a + 1000)]
                        xaccess = (\(x,_,_) -> x)
                        yaccess = (\(_,y,_) -> y)
                        zaccess = (\(_,_,z) -> z) 
                        xrange = toMinMax . S.map xaccess $ e
                        yrange = toMinMax . S.map yaccess $ e
                        zrange = toMinMax . S.map zaccess $ e
                        match access perm = S.size (S.intersection (S.map access e) (S.fromList perm)) >= 11
                        xresults = [(f, xoffset) | f <- rotations
                                                 , xoffset <- xrange
                                                 , match xaccess (map ((+xoffset) . xaccess . f) s)]
                        yresults = [(f, xoffset, yoffset) | (f, xoffset) <- xresults
                                                          , yoffset <- yrange
                                                          , match yaccess (map ((+yoffset) . yaccess . f) s)]
                        zresults = [(f, xoffset, yoffset, zoffset) | (f, xoffset, yoffset) <- yresults
                                                                   , zoffset <- zrange
                                                                   , match id (map (offset (xoffset, yoffset, zoffset) . f) s)]
                   in trace (show (length xresults) ++ " " ++ show (length yresults) ++ " " ++ show (length zresults)) $ [(map (offset (x0, y0, z0) . rot0) s, (x0, y0, z0)) | (rot0, x0, y0, z0) <- zresults]

        manhattanDistance :: [Xyz] -> Int
        manhattanDistance xs = maximum [md p1 p2 | p1 <- xs, p2 <- xs]

        md :: Xyz -> Xyz -> Int
        md (a, b, c) (d, e, f) = abs (a-d) + abs (b-e) + abs (c-f)

        offset :: Xyz -> Xyz -> Xyz
        offset (a, b, c) (d, e, f) = (a + d, b + e, c + f)

        oneAxisRotations :: [(Xyz -> Xyz)]
        oneAxisRotations = [ \(x, y, z) -> ( x,  y,  z)
                           , \(x, y, z) -> ( x,  z, -y)
                           , \(x, y, z) -> ( x, -y, -z)
                           , \(x, y, z) -> ( x, -z,  y)
                           ]
        
        axisPermutations :: [(Xyz -> Xyz)]
        axisPermutations =  [ \(x, y, z) -> ( x,  y,  z)
                            , \(x, y, z) -> (-x,  y, -z)
                            , \(x, y, z) -> ( y, -x,  z)
                            , \(x, y, z) -> (-y,  x,  z)
                            , \(x, y, z) -> ( z,  y, -x)
                            , \(x, y, z) -> (-z,  y,  x)
                            ]

        rotations :: [(Xyz -> Xyz)]
        rotations = [ f . g | f <- oneAxisRotations, g <- axisPermutations ]

        addScannerDataToEnv :: Environment -> ScannerData -> Environment
        addScannerDataToEnv e = S.union e . S.fromList

        parsein ::[String] -> [ScannerData]
        parsein = reverse . filter (not . null) . map (map toXyz) . splitWhen (isPrefixOf "---") . filter (/="")
                    
        toXyz :: String -> Xyz
        toXyz s = let (a,b) = span (/=',') s
                      (c,d) = span (/=',') (tail b)
                      (e,_) = span (/=',') (tail d)
                  in (read a :: Int, read c :: Int, read e :: Int)
