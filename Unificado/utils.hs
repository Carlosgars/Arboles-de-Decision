module Utils where

import Tipos
import Data.List
import Data.Maybe

aleft par = (Left $ fst par, Left $ snd par)
aright par = (Right $ fst par, Right $ snd par)

lengthDouble :: [a] -> Double
lengthDouble = fromIntegral.length

getL :: Either a b -> a
getL (Left x) = x

getR :: Either a b -> b
getR (Right x) = x

ordensindups :: (Ord a) => [a] -> [a]
ordensindups = map head . group . sort

maximo :: [(b,Double)] -> (b,Double) -> b
maximo [] y                   = fst y
maximo (x:xs) y 
           | (snd x) > (snd y) = maximo xs x
           | otherwise         = maximo xs y

ocurrencia :: (Eq a) => [a] -> a -> Double
ocurrencia [] a      = 0
ocurrencia (x:xs) a
           | a == x    = 1 + ocurrencia xs a
           | otherwise = ocurrencia xs a


elimina _ []                 = []
elimina x (y:ys) | x == y    = elimina x ys
                   | otherwise = y : elimina x ys

eliminaLista :: (Eq a) => [a] -> [a] -> [a]
eliminaLista [] ys = ys
eliminaLista _ [] = []
eliminaLista (x:xs) ys =
    elimina x (eliminaLista xs ys)

atributoObjetivo :: Ejemplo -> Atributo
atributoObjetivo = fst.snd

valorObjetivo :: Ejemplo -> ValorAtrib
valorObjetivo = snd.snd

valores :: Atributo -> [Ejemplo] -> [ValorAtrib]
valores _ [] = []
valores atributo (e:ejemplos) =
    [ snd x | x <- fst e, fst x == atributo ]
    ++
    valores atributo ejemplos

valorAtributo :: Ejemplo -> Atributo -> ValorAtrib
valorAtributo ejemplo atributo =
    snd $ head (filter (\x -> fst x == atributo) (fst ejemplo))
       
evaluarD :: [Ejemplo] -> String -> Discreto -> [Ejemplo]
evaluarD ejemplos valor atributo =
    let atrib = Left atributo 
    in [x | x <- ejemplos, (getL $ valorAtributo x atrib) == valor ]

evaluarC :: [Ejemplo] -> String -> Continuo -> [Ejemplo]
evaluarC ejemplos valor atributo =
    let u = fromJust $ umbral atributo
        atrib = (Right atributo)
    in if valor == "<="
    then filter (\x -> (getR$valorAtributo x atrib) <= u) ejemplos
    else if valor == ">"
    then filter (\x -> (getR$valorAtributo x atrib) > u) ejemplos
    else []

evaluar :: [Ejemplo]  -> Atributo -> String -> [Ejemplo]
evaluar ejemplos atributo valor =
    either (evaluarD ejemplos valor) (evaluarC ejemplos valor) atributo

