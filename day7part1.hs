import System.IO  
import Control.Monad

resolvese :: (Int, [Int]) -> Bool
resolvese (k,[]) = k==0
resolvese (k,x:xs) = x<= k && (resolvese (k-x, xs) || (k `mod` x == 0 && resolvese (k `div` x, xs)))

parser :: String -> (Int,[Int])
parser = (\(x:xs) -> (read $ takeWhile (/= ':') x, reverse $ read <$> xs)) . words

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print.sum$ map fst.filter resolvese $ parser <$> lines contents
    hClose handle