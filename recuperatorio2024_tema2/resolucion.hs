-- 1)
minimoNumero :: Int -> Int -> Int -- hago una funcion que devuelva el minimo entre dos numeros
minimoNumero x y | x <= y = x
                 | otherwise = y 

minimoLista :: [Int] -> Int -- hago una funcion que devuelva el minimo de una lista
minimoLista [x] = x
minimoLista [x1,x2] = minimoNumero x1 x2
minimoLista (x1:x2:xs) | x1 <= x2 = minimoLista (x1:xs)
                       | otherwise = minimoLista (x2:xs) 

minimo :: [[Int]] -> Int
minimo [x] = minimoLista x
minimo [x1,x2] = minimoNumero (minimoLista x1) (minimoLista x2)
minimo (x1:xs) = minimoNumero (minimoLista x1) (minimo xs)

-- 2)
cantidadApariciones :: Int -> [Int] -> Int -- hago una funcion que devuelva la cantidad de apariciones de un int en una lista de int
cantidadApariciones x [] = 0
cantidadApariciones x (y:ys) | x == y = 1 + cantidadApariciones x ys
                             | otherwise = cantidadApariciones x ys

aplanar :: [[Int]] -> [Int] -- hago una funcion que concatene todas las listas
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

eliminarTodos :: Int -> [Int] -> [Int] -- hago una funcion que elimine todas las apariciones de un integer en una lista de int
eliminarTodos x [] = []
eliminarTodos x (y:ys) | x == y = eliminarTodos x ys
                       | otherwise = [y] ++ eliminarTodos x ys  

eliminarTodosListas :: Int -> [[Int]] -> [[Int]] -- hago una funcion que elimine todas las apariciones de un int en una lista de listas de int
eliminarTodosListas x [] = []
eliminarTodosListas x (y:ys) = [eliminarTodos x y] ++ eliminarTodosListas x ys

cantidadDeElementos :: [Int] -> Int -- hago una funcion que me de la cantidad de elementos de una lista de int
cantidadDeElementos [] = 0
cantidadDeElementos [x] = 1
cantidadDeElementos (x:xs) = 1 + cantidadDeElementos xs

primerElem :: [Int] -> Int -- hago una funcion que devuelva el primer elemento de una lista
primerElem [x] = x
primerElem (x:xs) = x

repetidos :: [[Int]] -> [Int] 
repetidos [] = []
repetidos [[x]] = []
repetidos (x:xs) 
    | cantidadDeElementos x == 0 = repetidos xs
    | cantidadDeElementos (aplanar (x:xs)) /= 1 && cantidadApariciones (primerElem x) (aplanar (x:xs)) >= 2 = [primerElem x] ++ repetidos (eliminarTodosListas (primerElem x) (x:xs))
    | cantidadApariciones (primerElem x) (aplanar (x:xs)) < 2 = repetidos (eliminarTodosListas (primerElem x) (x:xs)) 
   -- en esta funcion no uso head porque me tiraba muchos errores, en su lugar reemplace el uso de head por la funcion primerElem

-- 3)
listaPorPosicion :: Int -> [[Int]] -> [Int] -- hago una funcion que, dado un indice, encuentre una lista en una lista listas de integers
listaPorPosicion _ [] = [] 
listaPorPosicion 1 (x:xs) = x
listaPorPosicion indice (x:xs) = listaPorPosicion (indice - 1) xs

numeroEnLista :: Int -> [Int] -> Int -- hago una funcion que encuentre un numero en una lista de integers, dado un indice
numeroEnLista 1 (x:xs) = x
numeroEnLista indice (x:xs) = numeroEnLista (indice - 1) xs

valoresDeCamino :: [[Int]] -> [(Int, Int)] -> [Int]
valoresDeCamino (x:xs) [] = []
valoresDeCamino (x:xs) (y:ys) = [numeroEnLista (snd y) (listaPorPosicion (fst y) (x:xs))] ++ valoresDeCamino (x:xs) ys

-- 4) 
sucesionCollatz :: Int -> [Int]
sucesionCollatz 1 = [1]
sucesionCollatz n | mod n 2 == 0 = [n] ++ sucesionCollatz (div n 2)
                  | mod n 2 == 1 = [n] ++ sucesionCollatz (3*n + 1) 

esCaminoCollatz :: [Int] -> Int -> Bool
esCaminoCollatz (x:xs) n = sucesionCollatz n == (x:xs)

