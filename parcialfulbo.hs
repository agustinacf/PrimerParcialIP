--VAMOS CAMPEÓN!
--En Exactas se está jugando un torneo de futbol y la facultad le pidió a los alumnos de IP programar algunas funcionalidades en Haskell.
--Los datos con los que contamos para esto son los nombres de los equipos que participan del torneo, los nombres de los goleadores de cada
--uno de dichos equipo, y la cantidad de goles convertidos por esos jugadores. Los nombres de los equipos y sus respectivos goleadores serán
--modelados mediante tuplas de tipo (String,String), donde la primera componente representa el nombre del equipo, y la segunda representa el
--nombre del goleador de dicho equipo.

--En los problemas en los cuales se reciban, como parámetros, secuencias _goleadoresPorEquipo_ y _goles_, cada posicion de la lista goles representará
--la cantidad de goles obtenidos por el goleador del equipo que se encuentra en esa misma posición de _goleadoresPorEquipo_.
--Por ejemplo si la lista goleadoresPorEquipo es [("Sacachispas","Robertino Giacomini"),("Fénix","Matias Dominguez")] y la lista goles es [3,5], eso indica
--que Robertino Giacomini metió 3 goles y Matias Dominguez metió 5.

----------------------------------------------------------------------------------------------------------------------------------------------------------

--1) Goles de no goleadores [1 punto]

--problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩, totalGolesTorneo: Z ): Z {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
--    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos}
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--2) Equipos Válidos [3 puntos]

--problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩): Bool{
--    requiere: {True}
--    asegura: {(res = True) <-> goleadoresPorEquipo no contiene nombres de clubes repetidos, ni goleadores repetidos, ni jugadores con nombre de club}
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--3) Porcentaje de Goles [3 puntos]

--problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): R {
--    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {Hay al menos un elemento de goles mayor estricto a 0}
--    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores}
--}

--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos numeros de tipo Int:
--division :: Int -> Int -> Float
--division a b = (fromIntegral a) / (fromIntegral b)

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--4) Botín de Oro [3 puntos]

--problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): String {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {|goles| > 0}
--    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles}
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--1) 
golesDeNoGoleadores :: [(String,String)] -> [Int] -> Int -> Int
golesDeNoGoleadores _ gs golestotales = golestotales - (sumarTodosGolesGoleadores gs)   --gs:lista de goles de los goleadores

sumarTodosGolesGoleadores :: [Int] -> Int
sumarTodosGolesGoleadores [] = 0
sumarTodosGolesGoleadores (x:xs) = x + sumarTodosGolesGoleadores xs

--hago una lista que sume los goles de todos los goleadores, asi en la funcion golesDeNoGoleadores resto a los goles totales
--los goles de los goleadores

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--2)
aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((a,b):xs) = a : b : aplanar xs

pertenece :: String -> [String] -> Bool
pertenece x [] = False 
pertenece x (y:ys) | x == y = True
                     | otherwise = pertenece x ys   

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

equiposValidos :: [(String, String)] -> Bool
equiposValidos xs = not (hayRepetidos(aplanar xs))

--hago una funcion que una las tuplas (aplanar), una funcion que me indique si un elemento pertenece o no a una secuencia (pertenece) y otra que diga 
--si en una secuencia hay elementos repetidos (hayRepetidos), luego implemento todo en la funcion equiposValidos diciendo que si no hay elementos 
--repetidos, la funcion es True

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--3) 
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

goles :: String -> [(String, String)] -> [Int] -> Int
goles j ((e,g):es) (c:cs) | j == g = c   --j:jugador, e:equipo, g:goleador, c:gol de jugador
                          | otherwise = goles j es cs

porcentajeDeGoles :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeGoles j es gs = division (goles j es gs) (sumarTodosGolesGoleadores gs)   --es:equipos, gs: goleadores

--la funcion goles busca al jugador por cada secuencia de equipo y goleador, luego en porcentaje de goles se hace una division entre los goles
--del goleador y la suma de todos los goles de los goleadores

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--4)
botinDeOro :: [(String, String)] -> [Int] -> String
botinDeOro [(_,j)] [_] = j   --en una lista vacia, j es el maximo
botinDeOro (x:x2:xs) (g:g2:gs) | g > g2 = botinDeOro (x:xs) (g:gs)   --cada x es una lista de [(Equipo,goleador)] y cada g son los goles
                               | otherwise = botinDeOro (x2:xs) (g2:gs) 