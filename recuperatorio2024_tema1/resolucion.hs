-- 1)
maximoLista :: [Int] -> Int -- hago una funcion que me de el maximo de una lista
maximoLista [x] = x
maximoLista (x1:x2:xs) 
    | x1 >= x2 = maximoLista (x1:xs)
    | x2 >= x1 = maximoLista (x2:xs)

maximo :: [[Int]] -> Int
maximo [x] = maximoLista (x)
maximo (x1:x2:xs) 
    | maximoLista x1 >= maximoLista x2 = maximo (x1:xs)
    | otherwise = maximo (x2:xs)   

-- 2)
cantidadApariciones :: Int -> [Int] -> Int -- hago una funcion que me indique la cantidad de apariciones de un numero en una lista de numeros
cantidadApariciones n [] = 0
cantidadApariciones n (x:xs)
    | n == x = 1 + cantidadApariciones n xs
    | otherwise = cantidadApariciones n xs

aplanar :: [[Int]] -> [Int] -- hago una funcion que concatene listas de numeros
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs

eliminarNumero :: Int -> [Int] -> [Int] -- hago una funcion que elimine un numero de una lista
eliminarNumero _ [] = []
eliminarNumero n (x:xs) | n == x = eliminarNumero n xs
                        | otherwise = [x] ++ eliminarNumero n xs

tuplasCantApariciones :: [Int] -> [(Int, Int)] -- hago una funcion que me de tuplas donde el primer elemento es un numero, y el segundo su cantidad de apariciones
tuplasCantApariciones [] = []
tuplasCantApariciones [x] = [(x, 1)]
tuplasCantApariciones (x:xs) = [(x, cantidadApariciones x (x:xs))] ++ tuplasCantApariciones (eliminarNumero x (x:xs))

maximoTuplas :: [(Int, Int)] -> Int -- hago una funcion que me indique el primer elemento de la tupla maxima en una lista de tuplas
maximoTuplas [(x,y)] = x
maximoTuplas ((x1,x2):(y1,y2):xs) | x2 >= y2 = maximoTuplas ((x1,x2):xs)
                                  | otherwise = maximoTuplas ((y1,y2):xs) 

masRepetido :: [[Int]] -> Int
masRepetido (x1:xs) = maximoTuplas(tuplasCantApariciones(aplanar (x1:xs)))

-- 3) 
valoresDeCamino :: [[Int]] -> [(Int, Int)] -> [Int]
