import System.IO  
import Control.Monad
import qualified Data.Set as Set
import Data.List (nub)

type AntSys = [(Int,Int)]

colineares :: (Real a) => (a,a)->(a,a)->(a,a)->Bool
colineares (a1,a2) (b1,b2) (c1,c2)
    |a1 == b1 = b1 == c1
    |a2 == b2 = b2 == c1
    |otherwise = toRational (c1-b1) / toRational (a1-b1) == toRational (c2-b2) / toRational (a2-b2) 

getChars :: [String] -> String
getChars ls =  Set.toList $ Set.delete '.' $ Set.unions $ (Set.fromList <$> ls)

getAntSys :: AntSys->[String] -> [AntSys]
getAntSys antennas ls = (\c -> [(i,j) | (i,j)<- antennas, (ls!!i)!!j == c]) <$> chars
        where chars = getChars ls

countANodes :: AntSys->(Int,Int) -> AntSys -> AntSys
countANodes antennas (n,m) antSys = (filter isANode) $ [(i,j) | i<-[0..n], j<- [0..m]]
    where isANode (i,j) = any (\(a1,a2)-> any (\(b1,b2)-> (b1,b2)/= (a1,a2) && colineares (a1,a2) (b1,b2) (i,j)) antSys) antSys
          

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let parsedCont = lines contents
    let (n,m) = (length parsedCont -1, length (head parsedCont) -1)
    let antennas = [(i,j) | i<-[0..n], j<- [0 ..m], (parsedCont!!i)!!j /= '.']
    (print.length.nub $ (foldl (++) []) $ countANodes antennas (n,m) <$> (getAntSys antennas parsedCont))    
    hClose handle
    