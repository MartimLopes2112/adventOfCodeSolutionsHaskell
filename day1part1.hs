
main = do
    contents <- getContents
    (print.(foldr (+) 0).(map (\x -> abs $ head x - head (tail x))).orderColumns.(map ((map read). words)).lines) contents

orderColumns ::(Ord a)=> [[a]]->[[a]]
orderColumns = transpose . (map qcksort) . transpose 

qcksort :: (Ord a)=> [a] -> [a]
qcksort []=[]
qcksort (x:xs) = (qcksort. (filter (<= x))$ xs) ++ [x] ++ (qcksort. (filter ( >x))$ xs)

transpose :: [[a]]-> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
