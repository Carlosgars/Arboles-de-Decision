module Utils where

import Tipos

etiqueta :: Ejemplo a b -> b
etiqueta = snd.snd

clasificacion :: (Ejemplo a b) -> (Atributo b)
clasificacion  = fst.snd


maximo :: [(b,Int)] -> (b,Int) -> b
maximo [] y = fst y
maximo (x:xs) y = if (snd x) > (snd y) then maximo xs x else maximo xs y

ocurrencia :: (Eq a) => a -> [a] -> Int
ocurrencia a [] = 0
ocurrencia a (x:xs) = if a == x then 1 + ocurrencia a xs else ocurrencia a xs


elimina _ []                 = []
elimina x (y:ys) | x == y    = elimina x ys
                    | otherwise = y : elimina x ys