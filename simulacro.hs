--Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, 
--que tendrán como objetivo representar algunas relaciones e interacciones entre los usuarios. 
--Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, 
--utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
--  requiere: {True}
--  asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas(1), ni tuplas con ambas componentes iguales}
--}
--(1) A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

--problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--  requiere: {relacionesValidas(relaciones)}
--  asegura: {res no  tiene elementos repetidos}
--  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
--}

--problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--  requiere: {relacionesValidas(relaciones)}
--  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
--}

--problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
--  requiere: {relaciones no vacía}
--  requiere: {relacionesValidas(relaciones)}
--  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas ((x1,x2):[]) = True
relacionesValidas ((x1,x2):y:ys) | esIgual x1 x2 || pertenece x1 y && pertenece x2 y || pertenece2 (x1,x2) (y:ys) = False
                                 | otherwise = relacionesValidas ((x1,x2):ys) 

esIgual :: String -> String -> Bool
esIgual x1 x2 | x1 == x2 = True
              | otherwise = False  

pertenece :: String -> (String, String) -> Bool
pertenece x (y1,y2) | x == y1 || x == y2 = True
                    | otherwise = False

pertenece2 :: (String, String) -> [(String, String)] -> Bool
pertenece2 (x1,x2) [] = False
pertenece2 (x1,x2) (y:ys) | pertenece x1 y && pertenece x2 y = True
                          | otherwise = pertenece2 (x1,x2) ys   

-----------------------------------------------------------------------------------------------------------------------------------------------------------

personas :: [(String, String)] -> [String]
personas [] = []
personas (x:xs) = eliminarRepetidos(aplanar (x:xs))

aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((x,y):ys) = [x] ++ [y] ++ aplanar ys

pertenece3 :: String -> [String] -> Bool
pertenece3 x [] = False
pertenece3 x (y:ys) | x == y = True
                    | otherwise = pertenece3 x ys

quitarTodos :: String -> [String] -> [String]
quitarTodos _ [] = []
quitarTodos elem (x:xs) | elem == x = quitarTodos elem xs
                        | otherwise = x : quitarTodos elem xs  

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece3 x xs = [x] ++ eliminarRepetidos(quitarTodos x xs) 
                         | otherwise = [x] ++ eliminarRepetidos xs   

-----------------------------------------------------------------------------------------------------------------------------------------------------------
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe elem ((x1,x2):xs) | pertenece elem (x1,x2) = [x1,x2] ++ amigosDe elem xs
                           | otherwise = amigosDe elem xs   

pertenece4 :: String -> [(String, String)] -> Bool
pertenece4 x [] = False
pertenece4 x ((y1,y2):ys) | pertenece x (y1,y2) = True
                          | otherwise = pertenece4 x ys

-----------------------------------------------------------------------------------------------------------------------------------------------------------

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [] = []
personaConMasAmigos (x:xs) = (aplanar(x:xs))