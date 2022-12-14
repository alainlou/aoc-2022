import Data.List (nub)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge xs ys

getTailPath :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
getTailPath [] _ = []
getTailPath (headXY:headPath) (tailX, tailY) =
    let
        (x, y) = headXY
        moveX = if x - tailX > 0 then 1 else if x - tailX < 0 then -1 else 0
        moveY = if y - tailY > 0 then 1 else if y - tailY < 0 then -1 else 0
        (tailX', tailY') =
            if abs (x - tailX) <= 1 && abs (y - tailY) <= 1
            then (tailX, tailY)
            else (tailX + moveX, tailY + moveY)
    in
        (tailX', tailY') : (getTailPath (headPath) (tailX', tailY'))

addMove :: [(Int, Int)] -> String -> [(Int, Int)]
addMove path line =
    let
        (dir, dist) = splitAt 1 line
        dist' = read dist :: Int
        (x, y) = last path
        path' = case dir of
            "U" -> zip (concat (replicate (dist'+1) [x])) [(y+1)..(y + dist')]
            "D" -> zip (concat (replicate (dist'+1) [x])) (reverse [(y - dist')..(y-1)])
            "L" -> zip (reverse [(x - dist')..(x-1)]) (concat (replicate (dist'+1) [y]))
            "R" -> zip [(x+1)..(x + dist')] (concat (replicate (dist'+1) [y]))
            _ -> error "Invalid input"
    in
        path ++ path'

getPath :: [String] -> [(Int, Int)]
getPath lns = foldl addMove [(0, 0)] lns

main :: IO ()
main = do
    input <- getContents
    let headPath = getPath (lines input)
    putStrLn $ (show . length . nub) $ foldl getTailPath headPath (concat (replicate 9 [(0, 0)]))
