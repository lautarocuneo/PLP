limpiar :: String -> String -> String
limpiar [] s = s
limpiar (x:xs) s = limpiar xs (eliminar x s) --clave esto. 

eliminar :: Char -> String -> String
eliminar _ [] = []
eliminar c (x:xs)
    | c == x = eliminar c xs
    | otherwise = x : eliminar c xs

    --eliminar simplemente crea una nueva lista sin el caracter deseado.


sumaLista :: [Float] -> Float
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

promedio :: [Float] -> Float
promedio [] = 0
promedio x =  (sumaLista x) / fromIntegral (length x)

difPromedio :: [Float] -> [Float]
difPromedio s = map (\x -> x - promedio s) s 

-- otra forma de escribir \x -> x - promedio s
--seria restarPromedio x s = x - promedio(s)


todosIguales :: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales (x:xs) 
    | null xs = True -- Si la cola es vacía, todos los elementos son iguales
    | x == head xs = todosIguales xs
    | otherwise = False

