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
relacionesValidas [] = True
relacionesValidas (unaAmistad:masLista) = not (mismaPersona unaAmistad) && not (amistadRepetida unaAmistad masLista) && pasoRecursivo
                                        where pasoRecursivo = relacionesValidas masLista

mismaPersona:: (String, String) -> Bool
mismaPersona (a,b) = a == b

amistadRepetida :: (String, String) -> [(String, String)] -> Bool
amistadRepetida _ [] = False
amistadRepetida a (b:elResto) = mismaAmistad a b || amistadRepetida a elResto
                                where mismaAmistad (a,b) (c,d) = (a==c && b == d) || (a==d && b == c)  

--la funcion mismaPersona indica si en una tupla, las dos componentes son iguales, en este caso representando que la misma persona se repite dos
--veces.
--la funcion amistadRepetida indica si dos tuplas (a,b) y (c,d) tienen las mismas componentes, caso contrario se hace la llamada recursiva de a
--con el resto de las tuplas.
--la funcion relacionesValidas corrobora que si no hay tuplas con la misma persona o tuplas repetidas, enntonces las relaciones son validas.

-----------------------------------------------------------------------------------------------------------------------------------------------------------

personas :: [(String, String)] -> [String]
personas [] = []
personas (x:xs) = eliminarRepetidos(aplanar (x:xs))

aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((x,y):ys) = [x] ++ [y] ++ aplanar ys

pertenece :: String -> [String] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                    | otherwise = pertenece x ys

quitarTodos :: String -> [String] -> [String]
quitarTodos _ [] = []
quitarTodos elem (x:xs) | elem == x = quitarTodos elem xs
                        | otherwise = x : quitarTodos elem xs  

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = [x] ++ eliminarRepetidos(quitarTodos x xs) 
                         | otherwise = [x] ++ eliminarRepetidos xs   

--se aplana la secuencia de tuplas de string (aplanar), y luego con las funciones pertenece, quitarTodos y eliminarRepetidos se eliminan aquellos
--nombres que puedan aparecer dos veces en la lista de strings.
--en la funcion personas se aplica la funcion eliminarRepetidos a la secuencia de tuplas de strings aplanada (con aplanar). 

-----------------------------------------------------------------------------------------------------------------------------------------------------------

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe elem ((x1,x2):xs) | elem == x1 = [x2] ++ amigosDe elem xs
                           | elem == x2 = [x1] ++ amigosDe elem xs 
                           | otherwise = amigosDe elem xs

--si el elemento es igual a x1, el elemento que se suma a la lista de strings es x2 y se realiza el paso recursivo con el resto de las tuplas.
--si el elemento es igual a x2, el elemento que se suma a la lista de strings es x1 y se realiza el paso recursivo con el resto de las tuplas.
--si elemento no es igual a ninguna componente de la primer tupla, se realiza el paso recursivo con el resto de tuplas de la secuencia.

-----------------------------------------------------------------------------------------------------------------------------------------------------------

personaConMasAmigos :: [(String, String)] -> String 
personaConMasAmigos relaciones =  personaConMasAmigosAux personasDeRelacion (cantidadDeAmigosDePersonas personasDeRelacion relaciones) 
                                    where personasDeRelacion = personas relaciones

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

cantidadDeAmigosDePersonas :: [String] -> [(String, String)] -> [Int]
cantidadDeAmigosDePersonas [] _ = []
cantidadDeAmigosDePersonas (p1:ps) relaciones = longitud (amigosDe p1 relaciones) : pasoRecursivo
                                                where pasoRecursivo = cantidadDeAmigosDePersonas ps relaciones
--p1:persona 1, ps:el resto de personas
--dada una lista de nombres y una lista de relaciones, el resultado da el numero de amigos de cada persona.

personaConMasAmigosAux :: [String] -> [Int] -> String
personaConMasAmigosAux [p1] _ = p1
personaConMasAmigosAux (p1:p2:ps) (c1:c2:cs) | c1 > c2 = personaConMasAmigosAux (p1:ps) (c1:cs)
                                             | otherwise = personaConMasAmigosAux (p2:ps) (c2:cs)   
--c1:cantidad de amigos 1, c2:cantidad de amigos 2
--dada una lista de personas y una lista con la cantidad de amigos de cada persona, da como resultado la persona con mayor cantidad de amigos
--de la lista.

--en cantidadDeAmigosDePersonas calculo la longitud de la lista de amigosDe p1 y las relaciones, y luego aplico el paso recursivo al resto de 
--personas de la lista con las relaciones.
--en personaConMasAmigosAux, si la cantidad de amigos 1 es mayor a la cantidad de amigos 2, realizo el paso recursivo con p1 y el resto de las
--personas y con c1 y el resto de la cantidad de amigos. en caso contrario, la cantidad de amigos 2 sera mayor a la cantidad de amigos 1
--y se realizara el paso recursivo con esos dos datos.