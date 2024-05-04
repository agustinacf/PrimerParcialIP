aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((x,y):ys) = [x] ++ [y] ++ aplanar ys

pertenece3 :: String -> [String] -> Bool
pertenece3 x [] = False
pertenece3 x (y:ys) | x == y = True
                    | otherwise = pertenece3 x ys

masApariciones :: [String] -> String
masApariciones [] = []
masApariciones (x:y:xs) | x == y = masApariciones xs 


