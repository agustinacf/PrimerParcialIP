sumaDeVotos :: [Int] -> Int
sumaDeVotos [] = 0 
sumaDeVotos (x:xs) = x + sumaDeVotos xs

votosEnBlanco :: [(String, String)] -> [Int] -> Int -> Int
votosEnBlanco ((p,vi):ps) (x:xs) totales = totales - sumaDeVotos (x:xs) 