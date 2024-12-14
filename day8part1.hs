import System.IO  
import Control.Monad
import qualified Data.Set as Set
import Data.List (nub)

type AntSys = [(Int,Int)]

getChars :: [String] -> String
getChars ls =  Set.toList $ Set.delete '.' $ Set.unions $ (Set.fromList <$> ls)

getAntSys :: AntSys->[String] -> [AntSys]
getAntSys antennas ls = (\c -> [(i,j) | (i,j)<- antennas, (ls!!i)!!j == c]) <$> chars
        where chars = getChars ls

countANodes :: AntSys->(Int,Int) -> AntSys -> AntSys
countANodes antennas (n,m) antSys = (filter isANode) $ [(i,j) | i<-[0..n], j<- [0..m]]
    where isANode (i,j) = any (\(a,b)-> (a,b)/=(i,j) && even (a+i) && even (b+j) && ((a+i) `div` 2,(b+j) `div` 2) `elem` antSys) antSys

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let parsedCont = lines contents
    let (n,m) = (length parsedCont -1, length (head parsedCont) -1)
    let antennas = [(i,j) | i<-[0..n], j<- [0 ..m], (parsedCont!!i)!!j /= '.']
    (print.length.nub $ (foldl (++) []) $ countANodes antennas (n,m) <$> (getAntSys antennas parsedCont))    
    hClose handle
    