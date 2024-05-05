{-

¡Vamos Campeones!
En exactas se está jugando un torneo de fútbol y la facultad le pidió a los alumnos de IP programar algunas
funcionalidades en Haskell, Los datos con los que contamos para esto son los nombres de los equipos que participan
del torneo, los nombres de los arqueros titulares de cada uno de dichos equipos, y la cantidad de goles recibidos
por esos arqueros. Los nombres de los equipos y sus respectivos arqueros serán modelados mediante tuplas de tipo
(String, String), donde la primera componente representa el nombre del equipo, y la segunda representa el nombre
del arquero titular de dicho equipo.
En los problemas en los cuales se reciben como parámetros secuencias arquerosPorEquipo y goles, cada posición de
la lista goles representará la cantidad de goles recibidos por el arquero del equipo que se encuentra en esa misma
posicion de arquerosPorEquipo. Por ejemplo, si la lista arquerosPorEquipo es [("Sacachispas", "Neyder Aragon"),
("Fenix", "Nahuel Galardi")] y la lista de goles es [3, 5], eso indicaría que Neyder Aragon recibió 3 goles, y
Nahuel Galardi 5.

Se pueden usar las siguientes funciones del preludio:
	- Listas: head, tail, last, init, length, elem, ++
	- Tuplas: fst, snd
	- Operaciones Lógicas: &&, ||, not
	- Constructores de listas: (x:xs), []
	- Constructores de tuplas: (x, y)

1) Atajaron Suplentes
problema atajaronSuplentes (arquerosPorEquipo: seq<String X String>, goles: seq<Z>, totalGolesTorneo: Z): Z {
	requiere: {equiposValidos(arquerosPorEquipo)
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
	asegura: {res es la cantidad de goles recibidos en el torneo por arqueros que no son titulares en sus equipos}
}

2) Equipos Válidos
problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool {
	requiere: {True}
	asegura: {(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club}
}

3) Porcentaje de goles
problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
	asegura: {res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares}
}

Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como float la división entre dos
numeros de tipo Int.

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

4) Valla Menos Vencida
problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {|goles| > 0}
	asegura: {res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles}
}

-}

----------------------------------------------------------------------------------------------------------------------------------------------

sumaGoles :: [Int] -> Int
sumaGoles [x] = x
sumaGoles (x:xs) = x + sumaGoles xs

atajaronSuplentes :: [(String, String)] -> [Int] -> Int -> Int
atajaronSuplentes _ (g:gs) golestotales = golestotales - sumaGoles (g:gs)   --(g:gs):goles recibidos por arqueros titulares

--a la suma de goles total del torneo le resto la suma de los goles recibidos por los arqueros titulares de cada equipo.

----------------------------------------------------------------------------------------------------------------------------------------------

aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((x,y):xs) = [x] ++ [y] ++ aplanar xs

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece x (y:xs) | x == y = True
                   | otherwise = pertenece x xs

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

equiposValidos :: [(String, String)] -> Bool
equiposValidos (e:es) = not (hayRepetidos(aplanar(e:es)))   --(e:es):tuplas de equipo y arquero titular

--primero aplano la lista de tuplas, luego implemento una funcion (hayRepetidos) para observar si en la lista hay elementos repetidos, y en la 
--funcion equiposValidos si no hay elementos repetidos, los equipos son validos, en caso contrario los equipos no seran validos.

----------------------------------------------------------------------------------------------------------------------------------------------

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

golesArquero :: String -> [(String, String)] -> [Int] -> Int
golesArquero arquero ((e,a):es) (g:gs) | arquero == a = g
									   | otherwise = golesArquero arquero es gs
--(e,a):(equipo, arquero titular)
--es:resto de equipos
--g:goles

porcentajeDeGoles :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeGoles arquero (e:es) (g:gs) = division (golesArquero arquero (e:es) (g:gs)) (sumaGoles (g:gs))

----------------------------------------------------------------------------------------------------------------------------------------------

vallaMenosVencida :: [(String, String)] -> [Int] -> String
vallaMenosVencida [(_,a)] [_] = a 
vallaMenosVencida (e1:e2:es) (g1:g2:gs) | g1 < g2 = vallaMenosVencida (e1:es) (g1:gs)
										| otherwise = vallaMenosVencida (e2:es) (g2:gs)	