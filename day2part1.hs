
parser :: String -> [[Int]]
parser = (map ((map read). words)).lines

diffsList :: [Int]-> [Int]
diffsList []        = []
diffsList [x]       = []
diffsList (x:xs)    = (head xs - x):diffsList xs

resolver :: [Int] -> Int
resolver inp
    |inp == []       = 0
    |length inp == 1 = 1
    |otherwise       = (\b -> if b then 1 else 0) $ boolToInt $ diffsList inp
                        where boolToInt = (if head (tail inp) > head inp then all (\diff -> 1<=diff &&diff<=3) else all (\diff -> 1<=(-diff)&&(-diff)<=3))

                                        

main = do
    contents <- getContents
    ( print.(foldr (+) 0).(map resolver) .parser) contents