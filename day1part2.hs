import Data.List (transpose)
import qualified Data.Map as Map (intersectionWith, fromList, foldr,lookup,Map) 

parser :: String -> [[Int]]
parser = (map ((map read). words)).lines

occourenceList :: (Ord a)=>[a]->[(a,Int)]
occourenceList [] = []
occourenceList (x:xs)= (occourenceList $ filter (<x) xs) ++ [(x,length (filter (==x) xs) +1)] ++ (occourenceList $ filter (>x) xs)

unMaybe :: Maybe Int -> Int
unMaybe Nothing = 0
unMaybe (Just x) = x

fazerConta :: [Int]-> Map.Map Int Int -> Int
fazerConta [] _ = 0
fazerConta (x:xs) m = x * (unMaybe $ Map.lookup x m) + (fazerConta xs m)

main = do
    contents <- getContents
    print $ uncurry fazerConta $ (\[l1,l2] -> (l1, Map.fromList $ occourenceList l2)) $ transpose $ parser contents