
votosPresidente :: String -> [(String, String)] -> [Int] -> Int
votosPresidente presidente ((p,vi):ps) (v:vs) | presidente == p = v   --(p,vi)= presidente, vicepresidente 
                                              | otherwise = votosPresidente presidente ps vs 


maximo :: [Int] -> Int
maximo [x] = x
maximo (x:y:xs) | x > y = maximo (x:xs)
                | otherwise = maximo (y:xs)

masVotado :: [Int] -> [(String, String)] -> String
masVotado (v:vs) ((p,vi):ps) | maximo (v:vs) == votosPresidente ( presidente ((p,vi):ps) v) = p