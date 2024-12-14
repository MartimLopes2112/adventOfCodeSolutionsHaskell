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

{-
checkTermination :: Matrix Char ->(Int,Int, Direction) -> Int -> Bool
checkTermination _ _ 0       = False
checkTermination m (x,y,d) n 
    |0<x' && x'<= (nrows m) && 0<y'&& y'<= (ncols m)  = checkTermination (fst newTick)  (snd newTick) (n-1)
    |otherwise                          = True
        where  (x',y') = updatePos (x,y) d
               newTick = tick m (x,y,d)-}

resolver1 :: Matrix Char -> (Matrix Char, (Int,Int))
--Recebe matriz do problema e retorna matriz no final da primeira alinea, juntamente com a posicao inicial
resolver1 m = (\(initM,(a,b)) -> (fst $ fixpoint iterFun (initM, (a,b,UpDir)), (a,b))).prepareInitialMatrix $ (m,(1,1))

resolver2 :: Matrix Char ->(Matrix Char,(Int,Int)) -> Int
--Recebe a matriz inicial, a matriz da parte 1 com pos inicial e retorna resposta
resolver2 mInic (m,(a,b)) = length $ filter (\(i,j) -> not $ checkTermination (setElem '#' (i,j) mInic, (a,b,UpDir)) (1,1)) $
        filter (\par-> par /= (a,b) && 'X' == m!par) $ (\k -> (1+ k `div` (ncols m), 1+ k `mod` (ncols m) )) <$> [0..(ncols m * nrows m)-1]
{-

checkTermination :: (Matrix Char, (Int,Int,Direction)) -> Bool
checkTermination (m,(x,y,d))
    |1<x' && x'< (nrows m) && 1<y'&& y'< (ncols m)= if d==newD && m!(x',y') == 'X' && then False else checkTermination (newM, (x',y',newD))
    |otherwise = True
    where  (newM,(x',y',newD))   = tick m (x,y,d)
           --(_,(x'',y'',_))       = tick newM (x',y',newD)
-}

checkTermination :: (Matrix Char, (Int,Int,Direction)) -> (Int,Int) -> Bool
checkTermination (m,(x,y,d)) (pathLength, streak)
    |streak > 2*pathLength = False
    |d/= newD = checkTermination (m, (x,y,newD)) (pathLength,streak)
    | 1<x' && x'< (nrows m) && 1<y'&& y'< (ncols m)= 
        checkTermination (newM, (x',y',d)) (if m!(x',y')=='X' then (pathLength,streak+1) else (pathLength+1,1))
    |otherwise = True
    where  (newM,(x',y',newD))   = tick m (x,y,d)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print.(\m -> (resolver2 m).resolver1 $ m).fromLists $ lines contents
    hClose handle

--THIS SHIT TAKES FOREVER
