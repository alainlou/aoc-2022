import Data.List (nub)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge xs ys

getTailPath :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getTailPath _ [] = []
getTailPath _ [_] = []
getTailPath (tailX, tailY) (headXY:headPath) =
    let
        (x, y) = head headPath
        (tailX', tailY') =
            if abs (x - tailX) > 1 || abs (y - tailY) > 1
            then headXY
            else (tailX, tailY)
    in
        (getTailPath (tailX', tailY') (headPath)) ++ [(tailX', tailY')]

processLine :: ([(Int, Int)], (Int, Int)) -> String -> ([(Int, Int)], (Int, Int))
processLine (tailXys, (xHead, yHead)) line =
    let
        (dir, dist) = splitAt 1 line
        dist' = read dist :: Int
        headPath = case dir of
            "U" -> zip (concat (replicate (dist'+1) [xHead])) [yHead..(yHead + dist')]
            "D" -> zip (concat (replicate (dist'+1) [xHead])) (reverse [(yHead - dist')..yHead])
            "L" -> zip (reverse [(xHead - dist')..xHead]) (concat (replicate (dist'+1) [yHead]))
            "R" -> zip [xHead..(xHead + dist')] (concat (replicate (dist'+1) [yHead]))
            _ -> error "Invalid input"
        tailPath = getTailPath (head tailXys) headPath
    in
        (merge tailPath tailXys, last headPath)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ (show . length . nub  . fst) (foldl processLine ([(0, 0)], (0, 0)) (lines input))
