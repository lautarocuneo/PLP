
valorAbsoluto :: Float -> Float
valorAbsoluto x  
    | x >= 0 = x
    | otherwise = -x

esBisiesto :: Int -> Bool
esBisiesto x
    | (x `mod` 4 == 0) && ((x `mod` 100 /= 0) || (x `mod` 400 == 0)) = True
    | otherwise = False

factorial :: Int -> Int
factorial 0 = 1
factorial x = factorial (x-1) * x

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 1 = 0 
cantDivisoresPrimos n 
    | esPrimo n = 1
    | otherwise = cantDivisoresPrimosAux n (n-1)


cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux n m
    | m == 1 = 0
    | n `mod` m == 0 && esPrimo m = 1 + cantDivisoresPrimosAux n (m-1)
    | otherwise = cantDivisoresPrimosAux n (m-1)

esPrimo :: Int -> Bool
esPrimo n
    | n <= 1    = False
    | otherwise = all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]