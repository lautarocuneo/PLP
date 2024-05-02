
-- Definición de la función para sumar dos números
suma :: Int -> Int -> Int
suma x y = x + y

-- Función principal (main)
main :: IO ()
main = do
    putStrLn "Ingrese el primer número:"
    num1 <- readLn
    putStrLn "Ingrese el segundo número:"
    num2 <- readLn
    let resultado = suma num1 num2
    putStrLn ("La suma de los números es: " ++ show resultado)