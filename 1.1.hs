import Data.List.Split
import System.IO

main = do
    interact (show . maximum . map (sum . map read) . splitWhen null . lines)
