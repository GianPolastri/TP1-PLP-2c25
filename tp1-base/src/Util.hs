module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = if length s == n then s else e ++ s
    where e = replicate (n - length s) ' '



-- alinearDerecha n s = if length s >= n then s else foldr (\p rec -> (++) (" " ++ p) rec) "" s;

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = actualizarElem' f xs n

actualizarElem' ::  (a -> a) -> [a] -> (Int -> [a])
actualizarElem' f = foldr (\x rec -> \n -> if n == 0 then f x :rec (-1) else x: rec (n-1)) (const [])
    
            {- 
                    
                la lambda es una funcion que devuelve otra lambda (:: Int -> a).

                Como la lambda devuelve una funcion de Int ->[a], a la recursion le tengo que pasar un Int 
                para que pueda continuar con su recursion. Como estamos haciendo n-1 es como recorrer la lista
                de der a izq (n denota el indice en la lista -posicion de x en la lista-). En la rama true pasamos
                un (-1) para no afectar a n.
                    
            -}
            
-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
