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
laQueMasHayQueCodificar frase _ = masRepeticiones frase frase

masRepeticiones :: [Char] -> [Char] -> Char
masRepeticiones [x] _ = x 
masRepeticiones (x:y:xs) frase | cantidadApariciones x frase > cantidadApariciones y frase = masRepeticiones (x:xs) frase
                               | cantidadApariciones x frase == cantidadApariciones y frase = masRepeticiones (x:xs) frase
                               | otherwise = masRepeticiones (y:xs) frase  

perteneceMapeo :: Char -> [Char] -> [(Char, Char)]


----------------------------------------------------------------------------------------------------------------------------------------------

codificarFrase :: [Char] -> [(Char, Char)] 



