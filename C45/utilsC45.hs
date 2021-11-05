module UtilsC45 where

import TiposC45
import Data.List
import Data.Maybe
import EjemplosC45

getL :: Either a b -> a
getL (Left x) = x

getR :: Either a b -> b
getR (Right x) = x

atributoObjetivo :: Ejemplo -> Atributo
atributoObjetivo = fst.snd

clasificacion :: Ejemplo -> ValorAtrib
clasificacion = snd.snd

valores :: Atributo -> [Ejemplo] -> [ValorAtrib]
valores _ [] = []
valores atributo (e:ejemplos) = [ snd x | x <- fst e, fst x == atributo ]
        ++ valores atributo ejemplos

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

maximo :: [(b,Int)] -> (b,Int) -> b
maximo [] y                   = fst y
maximo (x:xs) y 
           | (snd x) > (snd y) = maximo xs x
           | otherwise         = maximo xs y

ocurrencia :: (Eq a) => a -> [a] -> Int
ocurrencia a []     = 0
ocurrencia a (x:xs) 
           | a == x    = 1 + ocurrencia a xs
           | otherwise = ocurrencia a xs


elimina _ []                 = []
elimina x (y:ys) | x == y    = elimina x ys
                   | otherwise = y : elimina x ys


evaluarDiscreto :: [Ejemplo] -> String -> Discreto -> [Ejemplo]
evaluarDiscreto ejemplos valor atributo =
           let atrib = Left atributo 
           in [x | x <- ejemplos, (getL $ valorAtributo x atrib) == valor ]

evaluarContinuo :: [Ejemplo] -> String -> Continuo -> [Ejemplo]
evaluarContinuo ejemplos valor atributo =
           let u = fromJust $ umbral atributo
               atrib = (Right atributo)
           in
           if valor == "<="
           then [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= u ]
           else if valor == ">"
           then [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > u ]
           else ejemplos

-- either :: (Discreto -> [Ejemplo]) -> (Continuo -> [Ejemplo]) -> Atributo -> [Ejemplo]

evaluar :: [Ejemplo]  -> Atributo -> String -> [Ejemplo]
evaluar ejemplos atributo valor =
         either (evaluarDiscreto ejemplos valor) (evaluarContinuo ejemplos valor) atributo


valorAtributo :: Ejemplo -> Atributo -> ValorAtrib
valorAtributo ejemplo atributo =
       snd $ head (filter (\x -> fst x == atributo) (fst ejemplo))


