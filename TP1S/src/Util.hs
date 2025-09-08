module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = replicate (max 0 (n - length s)) ' ' ++ s

{-
Replicate x ' ' genera una cadena de x espacios, por lo que al hacer max 0 (n - length s) 
me aseguro que replicate no tome un negativo, por lo que replica x veces ' ' y le concatena s.
-}

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f = zipWith (\i x -> if i == n then f x else x) [0 ..]


{-
Ej funcionamiento:
actualizarElem 2 mult2 [4, 5, 6, 7, 8] = zipWith (\i x -> if i == 2 then mult2 x else x) [0 ..] [4, 5, 6, 7, 8]

- zipWith agarra un elem de cada lista y los combina en pares: (0,4), (1,5), (2,6), (3,7), (4,8)
- se verifica la condición \i x -> if i == 2 then mult2 x else x
- (2,6): if 2 == 2 then mult2 6 else 6 => 12
- devuelve como lista [4, 5, 12, 7, 8]
-}

-- https://learnyouahaskell.github.io/higher-order-functions.html#composition (ctrl+f zipwith)

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
