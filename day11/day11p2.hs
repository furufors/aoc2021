#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (reverse, sort)
import Data.List.Split (chunksOf)
data DumboP a = Flashed | Charging a deriving (Eq)
type Dumbo = DumboP Int
type Dumbos = [[Dumbo]]

instance Functor DumboP where  
    fmap f (Charging x) = Charging (f x)  
    fmap f Flashed = Flashed 

instance Ord a => Ord (DumboP a) where
    (Charging a) `compare` (Charging b) = a `compare` b
    (Flashed) `compare` (Charging _) = LT
    (Charging _) `compare` (Flashed) = GT

main = interact $ show . length . (\d -> takeWhile ((\(s,ds) -> s /= dumboCount ds))(iterate step d)) . parsein
    where
        step :: (Int , Dumbos) -> (Int, Dumbos)
        step (i, ds) = fmap resetFlashed . countFlashes i . flash . increase $ ds

        increase :: Dumbos -> Dumbos
        increase = map (map incCharge)
        incCharge :: Dumbo -> Dumbo
        incCharge = fmap (+1)
        toFlashed :: Dumbo -> Dumbo
        toFlashed = const Flashed
        xrange :: Dumbos -> [Int]
        xrange ds = [1..(length (head ds) - 2)]
        yrange :: Dumbos -> [Int]
        yrange ds = [1..(length ds - 2)]
        dumboCount :: Dumbos -> Int
        dumboCount ds = (length $ xrange ds) * (length $ yrange ds)

        flash :: Dumbos -> Dumbos
        flash ds = if inert ds then ds
                   else flash (blow ds)

        blow :: Dumbos -> Dumbos
        blow ds = let (x,y) = head [(x,y) | x <- xrange ds , y <- yrange ds, ds!!y!!x > Charging 9]
                  in flash $
                        replace2D incCharge (x-1,y-1) .
                        replace2D incCharge (x-1,  y) .
                        replace2D incCharge (x-1,y+1) .
                        replace2D incCharge (x  ,y-1) .
                        replace2D incCharge (x  ,y+1) .
                        replace2D incCharge (x+1,y-1) .
                        replace2D incCharge (x+1,  y) .
                        replace2D incCharge (x+1,y+1) .
                        replace2D toFlashed (x  ,  y) $ ds

        replace :: (a -> a) -> Int -> [a] -> [a]
        replace f 0 (x:xs) = (f x):xs
        replace f i (x:xs) = x : replace f (i-1) xs
        replace f i [] = []

        replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
        replace2D f (x,y) = replace (replace f x) y

        inert :: Dumbos -> Bool
        inert = all (all (\x -> x < Charging 10))

        countFlashes :: Int -> Dumbos -> (Int, Dumbos)
        countFlashes old ds = let hasFlashed = [()| x <- xrange ds, y <- yrange ds, ds!!y!!x == Flashed]
                              in (length hasFlashed, ds)

        resetFlashed :: Dumbos -> Dumbos
        resetFlashed ds = pad [[ update x y | x <- xrange ds] | y <- yrange ds]
            where update x y = if ds!!y!!x == Flashed then Charging 0 else ds!!y!!x 
        
        parsein :: String -> (Int, Dumbos)
        parsein = (\x -> (0,x)) . pad . map (map (Charging . read)) . map (chunksOf 1) . lines
        
        pad :: Dumbos -> Dumbos
        pad ds = let hrz = [take (length (head ds) + 2) (repeat Flashed)]
                 in hrz ++ map (\r -> [Flashed] ++ r ++ [Flashed]) ds ++ hrz
