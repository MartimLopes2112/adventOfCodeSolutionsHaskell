
parse :: String -> [(Int,Int)]
parse inp
    |null inp = []
    |take 4 inp == "mul(" = if ')' `elem` take 8 (drop 4 inp) then parsePar (takeWhile (/=')') (drop 4 inp)) : parse (tail inp) else parse (tail inp)
    |otherwise =  parse $ tail inp

parsePar :: String ->(Int,Int)
parsePar inp
    |all (/= ',') inp || not (length (takeWhile (/= ',') inp) `elem` [1,2,3])  || not(length (dropWhile (/= ',') inp) `elem` [2,3,4])    = (0,0)
    |all (`elem` ['0'..'9']) ((takeWhile (/= ',') inp) ++ tail (dropWhile (/= ',') inp))                   
            = (read $ takeWhile (/= ',') inp,read $ tail (dropWhile (/= ',') inp) )
    |otherwise = (0,0)


main = do
    contents <- getContents
    ( print.(foldr (+) 0). (map (\(a,b) -> a*b)) . parse) contents
