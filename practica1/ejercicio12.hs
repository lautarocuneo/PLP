data Polinomio a = X --tipo b (b es = Polinomio a)
            | Cte a --tipo (a -> b) --Cte :: a -> Polinomio a , constante tiene este tipo porque recibe un a y construye un Polinomio a,  que es
            | Suma (Polinomio a) (Polinomio a) -- tipo (b -> b -> b) , recibe Polinomio a, Polinomio a, y devuelve Polinomio a.
            | Prod (Polinomio a) (Polinomio a) -- tipo (b -> b -> b) , recibe Polinomio a, Polinomio a, y devuelve Polinomio a.

-- la regla es cambiar Polinomio a por b 



foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli casoX casoCte casoSuma casoProd poli = case poli of 
    X -> casoX --valo del caso base de X.
    Cte k -> casoCte k --valor del caso base de Cte k, es el mismo k.
    Suma p q -> casoSuma (rec p) (rec q) 
    Prod p q -> casoProd (rec p) (rec q)
    where rec = foldPoli casoX casoCte casoSuma casoProd 


evaluar1 :: Num t => t -> Polinomio t -> t
evaluar1 n poli = case poli of
       X -> n
       Cte k -> k
       Suma p q -> evaluar1 n p + evaluar1 n q
       Prod p q -> evaluar1 n p * evaluar1 n q


evaluar2 :: Num b => b -> Polinomio b -> b
evaluar2 n = foldPoli n id (+) (*)

--va la identidad en la constante porque quiero que me devuelva lo mismo que le paso como constante.

--hay que separar en casos, por ejemplo foldr separa en casos para la cabeza de la lista y para la recursion de lo que queda.
--en este caso tenemos dos casos base, casoX y casoCte, y dos acumuladores recursivos, casoSuma y casoProd.
-- y por ultimo el polinomio en si mismo.