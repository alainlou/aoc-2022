import Data.Char (digitToInt)

canBeSeen :: (Int, (Int, Int)) -> [(Int, (Int, Int))] -> Bool
canBeSeen tree trees =
    let
        thisRow = (fst . snd) tree
        thisCol = (snd . snd) tree
        canBeSeenOnRight = (null . filter(\(otherTreeHeight, (x, y)) -> x == thisRow && y > thisCol && otherTreeHeight >= fst tree)) trees
        canBeSeenOnLeft = (null . filter(\(otherTreeHeight, (x, y)) -> x == thisRow && y < thisCol && otherTreeHeight >= fst tree)) trees
        canBeSeenOnTop = (null . filter(\(otherTreeHeight, (x, y)) -> x < thisRow && y == thisCol && otherTreeHeight >= fst tree)) trees
        canBeSeenOnBot = (null . filter(\(otherTreeHeight, (x, y)) -> x > thisRow && y == thisCol && otherTreeHeight >= fst tree)) trees
    in
        canBeSeenOnRight || canBeSeenOnLeft || canBeSeenOnTop || canBeSeenOnBot

main :: IO ()
main = do
    input <- getContents
    let mat = (map (map (digitToInt)) . lines) input
    let indexedMat = zip (map (\xs -> zip xs [0..]) mat) [0..]
    let trees = concat $ map (\(trees, rowIdx) -> map (\point -> (fst point, (rowIdx, snd point))) trees) indexedMat
    putStrLn $ show $ foldl (\acc x -> if x then acc + 1 else acc) 0 $ map (\tree -> canBeSeen tree trees) trees
