-- Definición de foldNat
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ v 0 = v
foldNat f v n = f n (foldNat f v (n-1))

--cb es el valor del caso base, n es el nat, y f es la funcion que aplico a cada n
--luego decremento n-1 para acercarme al caso base.

-- Definición de potencia utilizando foldNat
potencia :: Integer -> Integer -> Integer
potencia base exponente = foldNat (\_ acc -> acc * base) 1 exponente

--no nos importa la " cabeza " en este caso para la funcion (\_ acc -> acc * base), ya que lo maneja directamente foldNat
-- luego lo que hacemos es pasar la funcion tal que dado el nat actual y el llamado recursivo, queremos multiplicar el acumulador por la base. 


