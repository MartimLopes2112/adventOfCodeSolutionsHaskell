import System.IO  
import Control.Monad
import Data.Matrix (Matrix, (!), getElem, ncols, nrows, setElem, toList, fromLists)

fixpoint :: (Eq a)=>(a->a) ->a ->a 
fixpoint f x |x == f x  = x 
             |otherwise = fixpoint f (f x) 

--(\(a,b) -> 0<=a< (nrows m) && 0<=b< (ncols m)) $ updatePos (x,y) d

data Direction = UpDir | RightDir | DownDir | LeftDir    deriving(Enum, Eq,Show)

updatePos ::(Int,Int) -> Direction -> (Int,Int)
updatePos (x,y) LeftDir    = (x,y-1)
updatePos (x,y) RightDir   = (x,y+1)
updatePos (x,y) UpDir      = (x-1,y)
updatePos (x,y) DownDir    = (x+1,y)

rotate :: Direction -> Direction
rotate LeftDir = UpDir
rotate d = succ d

tick :: Matrix Char -> (Int,Int,Direction) -> (Matrix Char, (Int,Int,Direction))
--marks the guards' position (except if it's a rotation) and returns their next position + direction
tick m (x,y,d)
    | m!(x',y') == '#'              = (m, (x,y,rotate d))
    |otherwise                      = (setElem 'X' (x,y) m, (x',y',d))
        where (x',y') = updatePos (x,y) d

iterFun :: (Matrix Char,(Int,Int,Direction)) -> (Matrix Char,(Int,Int,Direction))
iterFun (m,(x,y,d))
    |0<x' && x'<= (nrows m) && 0<y'&& y'<= (ncols m)  = tick m (x,y,d)
    |otherwise                          = (setElem 'X' (x,y) m,(x,y,d))
        where  (x',y') = updatePos (x,y) d

resolver :: (Matrix Char, (Int,Int,Direction)) -> Int
resolver (m, (x,y,d)) = length.(filter (=='X')).toList.fst$ fixpoint iterFun (m,(x,y,d))

prepareInitialMatrix :: (Matrix Char,(Int,Int)) -> (Matrix Char, (Int,Int))
prepareInitialMatrix (m, (x,y))
    |m!(x,y) == '^' = (setElem 'X' (x,y) m, (x,y))
    |y+1> ncols m = prepareInitialMatrix (m, (x+1, 1))
    |otherwise = prepareInitialMatrix (m, (x, y+1))


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print.resolver.(\(m,(x,y))->(m,(x,y,UpDir))).prepareInitialMatrix$ (\m -> (m,(1,1))).fromLists $ lines contents
    hClose handle
