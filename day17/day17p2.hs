#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
type Pos = (Int, Int)
type Spd = (Int, Int)
type State = (Pos, Spd)
type Trajectory = [State]

targetLX = 211
targetRX = 232
targetUY = -69
targetLY = -124

main = putStrLn $ show . length . filter passesTarget . 
       map (takeWhile validState . calculateTrajectory) $ possibleStartConditions
    where
        possibleStartConditions :: [State]
        possibleStartConditions = [((0, 0), (xspd, yspd)) | xspd <- [0..targetRX], yspd <- [targetLY..(-1*targetLY)]]

        calculateTrajectory :: State -> Trajectory
        calculateTrajectory s@((x,y),(vx,vy)) = s:calculateTrajectory ((x+vx,y+vy),(vx - signum vx, vy - 1))

        passesTarget :: Trajectory -> Bool
        passesTarget = any (insideTarget . fst)

        insideTarget :: Pos -> Bool
        insideTarget (x, y) =  x <= targetRX && x >= targetLX && y <= targetUY && y >= targetLY 

        validState :: State -> Bool
        validState ((x, y), (vx, vy)) | x > targetRX = False
        validState ((x, y), (vx, vy)) | y < targetLY = False
        validState ((x, y), (vx, vy)) = True

        findMaxY :: [Trajectory] -> Int
        findMaxY = maximum . concat . map (map toY)

        toY :: State -> Int
        toY ((_,y),_) = y
