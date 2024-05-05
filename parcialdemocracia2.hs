--1. Viva la democracia
--La elección periódica de los gobernantes es la base de los Estados Modernos. Este sistema, denominado ”democracia” (término
--proveniente de la antigua Grecia), tiene diferentes variaciones, que incluyen diferentes formas de elección del/a máximo/a
--mandatario/a. Por ejemplo, en algunos países se eligen representantes en un colegio electoral (EEUU). En otros se vota a
--los/as miembros del parlamento (España). En nuestro país elegimos de forma directa la fórmula presidencial (Presidente/a y
--Vicepresidente/a) cada 4 años.
--A continuación presentamos una serie de ejercicios que tienen como objetivo implementar funciones para sistema de escrutinio
--de una elección presidencial. Leer las descripciones y especificaciones e implementar las funciones requeridas en Haskell, utilizado
--solamente las herramientas vistas en clase.
--Las fórmulas presidenciales serán representadas por tuplas (String x String), donde la primera componente será el nombre del
--candidato a presidente, y la segunda componente será el nombre del candidato a vicepresidente.
--En los problemas en los cuales se reciban como parámetro secuencias de fórmulas y votos, cada posición de la lista votos
--representará la cantidad de votos obtenidos por la fórmula del parámetro formulas en esa misma posición. Por ejemplo, si la
--lista de fórmulas es [(”Juan Pérez”,”Susana García”), (”María Montero”,”Pablo Moreno”)] y la lista de votos fuera [34, 56], eso
--indicaría que la fórmula encabezada por María Montero obtuvo 56 votos, y la lista encabezada por Juan Pérez obtuvo 34 votos.

--1.1. Ejercicio 1 - Votos en Blanco
--problema votosEnBlanco(formulas : seq < String × String >, votos : seq < Z >, cantTotalVotos : Z) : Z{
--requiere : {formulas Validas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron }
--}

--1.2. Ejercicio 2 - Fórmulas Válidas
--problema formulasValidas(formulas : seq < String × String >) : Bool{
--requiere : {True}
--asegura : {(res = true) ↔ formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser 
--candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas) }
--}

--1.3. Ejercicio 3 - Porcentaje de Votos
--problema porcentajeDeVotos(presidente : String, formulas : seq < String × String >, votos : seq < Z >) : R{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--asegura : {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
--}
--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo
--Int:
--division :: Int → Int → Float
--division a b = (fromIntegral a) / (fromIntegral b)

--1.4. Ejercicio 4 - Próximo Presidente
--problema proximoPresidente(formulas : seq < String × String >, votos : seq < Z >) : String{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--requiere : {|formulas| > 0}
--asegura : {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
--}

----------------------------------------------------------------------------------------------------------------------------------------------------------

sumaDeVotos :: [Int] -> Int
sumaDeVotos [] = 0 
sumaDeVotos (x:xs) = x + sumaDeVotos xs

votosEnBlanco :: [(String, String)] -> [Int] -> Int -> Int
votosEnBlanco _ (x:xs) totales = totales - sumaDeVotos (x:xs)   --(x:xs) son los votos afirmativos

--hago una funcion que sume todos los votos asignados a los distintos candidatos (sumaDeVotos) y luego, en la funcion votosEnBlanco, me da igual
--la secuencia de tuplas de candidatos, y le resto a los votos totales (totales) los votos asignados a los candidatos (sumaDeVotos (x:xs)).

----------------------------------------------------------------------------------------------------------------------------------------------------------

aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((x,y):xs) = [x] ++ [y] ++ aplanar xs

pertenece :: String -> [String] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys 

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = False
formulasValidas ((x,y):xs) = not (hayRepetidos (aplanar ((x,y):xs)))

--sabemos que un nombre debe aparecer una unica vez, por lo que implemento la funcion aplanar, para que me de una secuencia de strings con todos los
--nombres de las tuplas. implemento la funcion hayRepetidos que me indica si en una secuencia de strings hay elementos repetidos.
--en la funcion formulasValidas indico que si NO hay repetidos las formulas son validas (ya que not false = true) y si hay elementos repetidos, las
--formulas no son validas (ya que not true = false).

----------------------------------------------------------------------------------------------------------------------------------------------------------

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

votosPresidente :: String -> [(String, String)] -> [Int] -> Int
votosPresidente presidente ((p,vi):ps) (v:vs) | presidente == p = v   --(p,vi)= presidente, vicepresidente 
                                              | otherwise = votosPresidente presidente ps vs    

porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float 
porcentajeDeVotos presidente ((p,vi):ps) (v:vs) = division (votosPresidente presidente ((p,vi):ps) (v:vs)) (sumaDeVotos (v:vs))

--la funcion division fue dada en la consigna. implemento la funcion votosPresidente que me indica, dando el nombre de un presidente, cual fue la
--cantidad de votos que obtuvo.
--en la funcion porcentajeDeVotos divido la cantidad de votos del presidente con la suma de los votos asignados a una tupla, usando la funcion
--sumaDeVotos del primer ejercicio.

----------------------------------------------------------------------------------------------------------------------------------------------------------

proximoPresidente :: [(String, String)] -> [Int] -> String
proximoPresidente [(p,_)] [_] = p 
proximoPresidente (p1:p2:ps) (v1:v2:vs) | v1 > v2 = proximoPresidente (p1:ps) (v1:vs)
                                        | otherwise = proximoPresidente (p2:ps) (v2:vs)

--en la funcion proximoPresidente tenemos como caso base que el proximo presidente en una lista de una sola tupla y en una lista con un solo elemento
--sera p. luego, si los votos 1 son mayores que los votos 2, se hace la llamada recursiva con la primer tupla de candidatos y la primer cantidad de
--votos, en caso contrario se hara la llamada recursiva con la segunda tupla de candidatos y la segunda cantidad de votos.