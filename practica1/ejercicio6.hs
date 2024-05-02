recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--recursion primitiva, que toma una funcion un valor de caso base , y una lista, y aplica recursion al  

--dado elemento y lista devuelve el resultado de eliminar de la lista la primera aparicion del elemento si esta presente.
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else x:r) []
--o sea , si x == e , devuelvo la cola, si no es igual pego x al caso recursivo.

-- no es adecuado el foldr en este problema porque no tengo acceso a la cola de la lista, solo al llamado recursivo. 
-- por lo tanto nunca podria mi funcion devolver simplemente la cola en caso de encontrar el elemento, solo puede devolver el llamado recursivo.


insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if e >= x && (null xs || e <= head xs) then x:e:xs else x:r) [e]

--o sea la funcion dice, si e es mayor a x , y x e es mayor a la cabeza de xs o es null xs, entonces meto e en el medio, si no sigo iterando x sobre el llamado recursivo.