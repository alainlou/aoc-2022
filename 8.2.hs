import Data.Char (digitToInt)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) =
    let
        xDiff = abs (x1 - x2)
        yDiff = abs (y1 - y2)
    in
        xDiff + yDiff

numTreesSeen :: (Int, (Int, Int)) -> [(Int, (Int, Int))] -> Int
numTreesSeen srcTree treeList =
    let
        blockingTrees = filter (\otherOtherTree -> (fst otherOtherTree) >= (fst srcTree)) treeList
    in
        sum $
        map (\otherTree ->
            if null $ filter (\blockingTree -> (distance (snd blockingTree) (snd srcTree)) < (distance (snd otherTree) (snd srcTree))) blockingTrees then
                1
            else
                0
        ) treeList


scenicScore :: (Int, (Int, Int)) -> [(Int, (Int, Int))] -> Int
scenicScore tree trees =
    let
        thisRow = (fst . snd) tree
        thisCol = (snd . snd) tree
        seenOnRight = numTreesSeen tree (filter(\(_, (x, y)) -> x == thisRow && y > thisCol) trees)
        seenOnLeft = numTreesSeen tree (filter(\(_, (x, y)) -> x == thisRow && y < thisCol) trees)
        seenOnBot = numTreesSeen tree (filter(\(_, (x, y)) -> x < thisRow && y == thisCol) trees)
        seenOnTop = numTreesSeen tree (filter(\(_, (x, y)) -> x > thisRow && y == thisCol) trees)
    in
        seenOnRight * seenOnLeft * seenOnBot * seenOnTop

main :: IO ()
main = do
    input <- getContents
    let mat = (map (map (digitToInt)) . lines) input
    let indexedMat = zip (map (\xs -> zip xs [0..]) mat) [0..]
    let trees = concat $ map (\(trees, rowIdx) -> map (\point -> (fst point, (rowIdx, snd point))) trees) indexedMat
    putStrLn $ show $ (maximum . (map (\tree -> scenicScore tree trees))) trees
