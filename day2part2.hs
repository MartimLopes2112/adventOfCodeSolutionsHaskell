
parser :: String -> [[Int]]
parser = (map ((map read). words)).lines

diffsList :: [Int]-> [Int]
diffsList []        = []
diffsList [x]       = []
diffsList (x:xs)    = (head xs - x):diffsList xs

resolver :: [Int] -> Bool
resolver inp
    |inp == []       = False
    |length inp == 1 = True
    |otherwise       =  boolToInt $ diffsList inp
                        where boolToInt = (if head (tail inp) > head inp then all (\diff -> 1<=diff &&diff<=3) else all (\diff -> 1<=(-diff)&&(-diff)<=3))

removerBadLevel :: Bool ->[Int] -> [Int]
removerBadLevel _ [] = []
removerBadLevel _ [x] = [x]
removerBadLevel True (x:xs) = if diff<1 || 3<diff then x:tail xs else x:removerBadLevel True xs
                                where diff = head xs - x
removerBadLevel False (x:xs) = if diff>(-1) || (-3)>diff then x:tail xs else x:removerBadLevel False xs
                                where diff = head xs - x

resolver2 :: [Int] -> Bool
resolver2 inp
    |inp == []       = False
    |length inp == 1 = True
    |otherwise       = (resolver $ removerBadLevel False inp ) || (resolver $ removerBadLevel True inp ) || resolver (tail inp)

main = do
    contents <- getContents
    ( print. (foldr (+) 0).(map ((\b -> if b then 1 else 0) .resolver2)). parser) contents