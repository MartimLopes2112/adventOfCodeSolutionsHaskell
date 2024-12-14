import System.IO  
import Control.Monad


resolvese :: (Int, [Int]) -> Bool
resolvese (k,[]) = k==0
resolvese (k,[x]) = k==x
resolvese (k,x:xs)
 |x>k       = False
 |otherwise = resolvese (k, (x+ head xs):(tail xs)) || resolvese (k, (x*head xs):(tail xs))
           || resolvese (k,concatNum x (head xs): (tail xs))


parser :: String -> (Int,[Int])
parser = (\(x:xs) -> (read $ takeWhile (/= ':') x, read <$> xs)) . words


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print.sum$ map fst . filter resolvese $ parser <$> lines contents
    hClose handle

concatNum :: Int->Int->Int
concatNum a b = read $ show a ++ show b
