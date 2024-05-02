arbolEjemplo :: AB Int
arbolEjemplo = Bin (Bin Nil 2 Nil) 1 (Bin Nil 3 Nil)


vacioAB :: AB a -> Bool
vacioAB Nil = True 
vacioAB _ = False

--si paso algo NIL es true, en cualquier otro caso es false. 

negacionAB :: AB Bool → AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq valor der) = Bin (negacionAB izq) (not valor) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq valor der) = valor * productoAB izq * productoAB der



sacarBlancosPrincipioFinal (x:xs) | x == ' ' && last xs /= ' ' = xs
                                  | x == ' ' && last xs /= ' ' = init xs 
                                  | x /= ' ' && last xs /= ' ' = (x:xs)
                                 
