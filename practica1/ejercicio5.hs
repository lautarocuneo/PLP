{-Ejercicio 5 ⋆
Considerar las siguientes funciones:
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
then [x]
else x : elementosEnPosicionesPares (tail xs)
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)
Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
En caso contrario, explicar el motivo.-}

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
then [x]
else x : elementosEnPosicionesPares (tail xs)

--pidiendo null sobre la cola de la lista, la estoy usando por lo que es recursion explicita y no estructural.

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)

entrelazar2 :: [a] -> [a] -> [a]
entrelazar2 = foldr(\x f -> (\ys -> if null ys then [x] else x : f (tail ys))) id
--aca si es estructural digo yo. 

