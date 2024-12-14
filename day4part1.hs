--Ola eu do futurooooo!!!!!! Para instalar packages deves fazer 'cabal install --lib random' (no caso de random), se n fizeres isto
--o ghc n vai saber onde estao os packages. Xau bestieeee<3

import Data.Matrix (fromLists, (!), transpose, nrows,ncols, matrix, Matrix, getRow,toLists)
import qualified Data.Vector (toList)
import System.IO  
import Control.Monad

ninety :: Matrix a -> Matrix a
ninety m = transpose $ matrix (nrows m) (ncols m) (\(i,j) -> m!(i,(ncols m) + 1 -j))

getCombsList :: String -> Int
getCombsList [] = 0
getCombsList ('X':xs) = if take 3 xs == "MAS" then 1 + getCombsList (drop 3 xs) else getCombsList xs
getCombsList (x:xs) = getCombsList xs


checkDiag :: Matrix Char -> (Int,Int)->Bool
checkDiag m (i,j)
        | nrows m >= i+3 && ncols m >= j+3  = [m!(i,j),m!(i+1,j+1),m!(i+2,j+2),m!(i+3,j+3)] == "XMAS"
        | otherwise                      = False     

getCombRowDiag :: Matrix Char -> Int
getCombRowDiag m = (length . (filter (checkDiag m)) $ map (\k -> ((1+)$ k `div` (ncols m),(1+)$ k `mod` (ncols m)) )  [0.. (nrows m)*(ncols m)-1] )
                + (sum $ (map (getCombsList. Data.Vector.toList .((flip getRow) m))) [1..nrows m])

resolver m = sum $ take 4 $ map getCombRowDiag $ iterate ninety m

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    (print. resolver. fromLists .lines) contents
    hClose handle   