#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Set as S
import Data.List (isPrefixOf, reverse)
import Data.List.Split (splitWhen)
import Debug.Trace (trace)
type Xyz = (Int, Int, Int)
type ScannerData = [Xyz]
type Environment = S.Set Xyz

main = interact $ show . S.size . (\x -> trace (show x) $ x) . reconstruct . parsein . lines
    where
        reconstruct :: [ScannerData] -> Environment
        reconstruct (s1:ss) = let m = addScannerDataToEnv S.empty s1 in reconstructScanner m [] ss

        reconstructScanner :: Environment -> [ScannerData] -> [ScannerData] -> Environment
        reconstructScanner e [] [    ] = e
        reconstructScanner e (b:bs) [] = reconstructScanner (S.fromList b) [] ((S.toList e):bs)
        reconstructScanner e bs (s:ss) = trace (show (length ss + length bs + 1)) $
                                         let res = align e s
                                         in if res == []
                                            then reconstructScanner e (s:bs) ss
                                            else let e' = S.union e (S.fromList . head $ res)
                                                 in reconstructScanner e' [] (bs++ss)

        align :: Environment -> ScannerData -> [ScannerData]
        align e s = let toMinMax = \a -> [(S.findMin a - 1000)..(S.findMax a + 1000)]
                        xaccess = (\(x,_,_) -> x)
                        yaccess = (\(_,y,_) -> y)
                        zaccess = (\(_,_,z) -> z) 
                        xrange = toMinMax . S.map xaccess $ e
                        yrange = toMinMax . S.map yaccess $ e
                        zrange = toMinMax . S.map zaccess $ e
                        match access perm = S.size (S.intersection (S.map access e) (S.fromList perm)) >= 12
                        xresults = [(f, xoffset) | f <- rotations
                                                 , xoffset <- xrange
                                                 , match xaccess (map ((+xoffset) . xaccess . f) s)]
                        yresults = [(f, xoffset, yoffset) | (f, xoffset) <- xresults
                                                          , yoffset <- yrange
                                                          , match yaccess (map ((+yoffset) . yaccess . f) s)]
                        zresults = [(f, xoffset, yoffset, zoffset) | (f, xoffset, yoffset) <- yresults
                                                                   , zoffset <- zrange
                                                                   , match id (map (offset (xoffset, yoffset, zoffset) . f) s)]
                   in trace (show (length xresults) ++ " " ++ show (length yresults) ++ " " ++ show (length zresults)) $ [map (offset (x0, y0, z0) . rot0) s | (rot0, x0, y0, z0) <- zresults]

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
