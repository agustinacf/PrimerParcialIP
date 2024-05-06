hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c (x:xs) | c == fst x = True
                         | otherwise = hayQueCodificar c xs   

----------------------------------------------------------------------------------------------------------------------------------------------

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char, Char)] -> Int
cuantasVecesHayQueCodificar c frase mapeo | hayQueCodificar c mapeo == False = 0
                                          | hayQueCodificar c mapeo == True = cantidadApariciones c frase

cantidadApariciones :: Char -> [Char] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones c (x:xs) | c == x = 1 + cantidadApariciones c xs
                             | otherwise = cantidadApariciones c xs       

----------------------------------------------------------------------------------------------------------------------------------------------

laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> Char




