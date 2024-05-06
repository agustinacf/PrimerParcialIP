cantidadApariciones :: Char -> [Char] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones c (x:xs) | c == x = 1 + cantidadApariciones c xs
                             | otherwise = cantidadApariciones c xs   

masRepeticiones :: [Char] -> [Char] -> Char
masRepeticiones [x] _ = x 
masRepeticiones (x:y:xs) frase | cantidadApariciones x frase > cantidadApariciones y frase = masRepeticiones (x:xs) frase
                               | cantidadApariciones x frase == cantidadApariciones y frase = masRepeticiones (x:xs) frase
                               | otherwise = masRepeticiones (y:xs) frase  

