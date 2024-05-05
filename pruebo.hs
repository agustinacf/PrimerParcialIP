pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece x (y:xs) | x == y = True
                   | otherwise = pertenece x xs

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs