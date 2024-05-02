--concat map hace un map y concatena todos los resultados, es util cuando mi funcion mapeada genera listas adentro de la lista y quiero aplanar el resultado.
--take toma un numero entero n y una lista y devuelve los primeros n elelementos de la lista.
--drop esta funcion tambien toma un entero n y una lista y devuelve el resto de la lista despues de los primeros n.
--concatmap aplica una funcion a cada elemento de la lista.... deberia aplicar la funncion recursiva ??? y luego concatena resultados...

permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\x -> map (x:) (permutaciones (filter (/=x) xs))) xs
{-
Primero, la función verifica si la lista es vacía. Si lo es, devuelve una lista que contiene una lista vacía.
En nuestro caso, la lista no está vacía, por lo que pasamos a la siguiente línea.
Luego, la función utiliza concatMap para aplicar una función a cada elemento de la lista y concatenar los resultados.
En nuestro caso, la lista es [1,2], por lo que concatMap aplicará la función a 1 y 2.
La función que se aplica a cada elemento es (\x -> map (x:) (permutaciones (filter (/=x) xs))).
Esta función toma un elemento x y mapea la función (x:) a todas las permutaciones de la lista sin el elemento x.
Para x = 1, la función filter (/=1) xs devuelve [2].
Luego, llamamos a permutaciones [2], que devuelve [[2]] porque la única permutación de una lista con un solo elemento es la lista misma.
Luego, mapeamos la función (1:) a [[2]], lo que nos da [[1,2]].
Hacemos lo mismo para x = 2, obteniendo [[2,1]].
Finalmente, concatMap concatena estos resultados, dando como resultado [[1,2],[2,1]], que son todas las permutaciones de la lista [1,2].
-}



partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = concatMap (\ys -> [x:ys, ys]) (partes xs)
-- pensarlo como tda, es igual.

prefijos :: [a] -> [[a]]
prefijos xs = map (\n -> take n xs) [0..length xs]

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ agregarATodas x (sublistas xs)

--sublistas , concatena la cola de la lista con agregar x a 

agregarATodas :: a -> [[a]] -> [[a]]
agregarATodas x [] = []
agregarATodas x (ys:yss) = (x:ys) : agregarATodas x yss


--agregarATodas agrega x a cada sublista de la lista de listas.

                --quiero la funcion que haga take n veces incrementando.