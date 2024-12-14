import Data.Matrix (fromLists, (!), transpose, nrows,ncols, matrix, Matrix, getRow,toLists)
import qualified Data.Vector (toList)
import System.IO  
import Control.Monad

ninety :: Matrix a -> Matrix a
ninety m = transpose $ matrix (nrows m) (ncols m) (\(i,j) -> m!(i,(ncols m) + 1 -j))

checkDiag :: Matrix Char -> (Int,Int)->Bool
checkDiag m (i,j)
        | nrows m >= i+2 && ncols m >= j+2  = cross == "MMASS"
        | otherwise                      = False
        where cross = [m!(i,j),m!(i,j+2),m!(i+1,j+1),m!(i+2,j),m!(i+2,j+2)]  

getDiag::Matrix Char -> Int
getDiag m = length . (filter (checkDiag m)) $ map (\k -> ((1+)$ k `div` (ncols m),(1+)$ k `mod` (ncols m)) )  [0.. (nrows m)*(ncols m)-1]

resolver m = sum $ take 4 $ map getDiag $ iterate ninety m

main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    (print. resolver. fromLists .lines) contents
    hClose handle