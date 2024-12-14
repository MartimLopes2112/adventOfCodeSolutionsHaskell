import System.IO  
import Control.Monad

type Registo = [[Int]]  

fixpoint :: (Eq a)=>(a->a) ->a ->a 
fixpoint f x |x == f x  = x 
             |otherwise = fixpoint f (f x) 

eMenor :: Registo ->(Int,Int)->Bool
eMenor r (a,b) = b `elem` (r!!a)

construirRegisto :: [String] -> (Registo, [String])
construirRegisto inp
    |head inp == "" = (map (\x-> []) [0..99], tail inp)
    |otherwise      = ((take a oldReg) ++ [b:(oldReg!!a)] ++ (drop (a+1) oldReg), remainingStr)
                    where (oldReg,remainingStr) = construirRegisto (tail inp)
                          a = read (take 2 (head inp))
                          b = read (drop 3 (head inp))

avaliarSerie :: Registo -> [Int] -> Bool
avaliarSerie _ [x] = True
avaliarSerie r (x:xs) = eMenor r (x, head xs) && avaliarSerie r xs

retornarMeio :: Registo -> [Int] -> Int
retornarMeio r l = if not $ avaliarSerie r l then (fixpoint (tentarOrdenar r) l)!!(length l `div` 2) else 0

tentarOrdenar :: Registo -> [Int] -> [Int]
tentarOrdenar _ [] = []
tentarOrdenar _ [x] = [x]
tentarOrdenar r (x:xs) = if eMenor r (x,head xs) then x:tentarOrdenar r xs else head xs : (x : tail xs) 

parseSeries :: String -> [Int]
parseSeries ""        = []
parseSeries (',':xs)  = parseSeries xs
parseSeries inp       = (read $ take 2 inp):(parseSeries $ drop 2 inp)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let (r,inp) = construirRegisto $ lines contents
    print.sum.(map $ (retornarMeio r).parseSeries) $ inp 
    hClose handle

