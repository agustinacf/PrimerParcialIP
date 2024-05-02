algunoEs0 :: Float -> Float -> Bool  
algunoEs0 x y | x == 0 || y == 0 = True 
              | otherwise = False  

---------------------------------------------

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False

---------------------------------------------

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && y == z && x == z = x 
                    | x /= y && y /= z && x /= z = x + y + z
                    | x /= y && y /= z && x == z = x + y
                    | x /= y && y == z && x /= z = x + z
                    | x == y && y /= z && x /= z = y + z    

---------------------------------------------

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | x == y = True
                 | mod x y == 0 = True
                 | otherwise = False

---------------------------------------------

digitoUnidades :: Integer -> Integer 
digitoUnidades x = mod (abs x) 10

---------------------------------------------

digitoDecenas :: Integer -> Integer
digitoDecenas x = digitoUnidades(div (abs x) 10)