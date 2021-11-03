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
maximo [] y = fst y
maximo (x:xs) y = if (snd x) > (snd y) then maximo xs x else maximo xs y

ocurrencia :: (Eq a) => a -> [a] -> Int
ocurrencia a [] = 0
ocurrencia a (x:xs) = if a == x then 1 + ocurrencia a xs else ocurrencia a xs


elimina _ []                 = []
elimina x (y:ys) | x == y    = elimina x ys
                    | otherwise = y : elimina x ys

entropia :: [Ejemplo] -> Double
entropia [] = 0
entropia ejemplos =
         entropiaaux ejemplos (posiblesvalores $ getL $ atributoObjetivo $ head ejemplos) 0

entropiaaux :: [Ejemplo] -> [String] -> Double -> Double
entropiaaux ejemplos [] ac = ac
entropiaaux ejemplos (c:posiblesvalores) ac =
            let p = (fromIntegral (ocurrencia c (map (getL.snd.snd) ejemplos))) / (fromIntegral (length ejemplos))
            in
            if p > 0
            then
            entropiaaux ejemplos posiblesvalores (ac - p * (logBase (fromIntegral 2)  p))
            else entropiaaux ejemplos posiblesvalores 0


evaluarDiscreto :: [Ejemplo] -> String -> Discreto -> [Ejemplo]
evaluarDiscreto ejemplos valor atributo =
           let atrib = Left atributo 
           in [x | x <- ejemplos, (getL $ valorAtributo x atrib) == valor ]

evaluarContinuo :: [Ejemplo] -> String -> Continuo -> [Ejemplo]
evaluarContinuo ejemplos "<=" atributo =
           let u = fromJust $ umbral atributo
           in [ x | x <- ejemplos, (getR $ valorAtributo x (Right atributo)) <= u ]
evaluarContinuo ejemplos ">" atributo =
           let u = fromJust $ umbral atributo
           in [ x | x <- ejemplos, (getR $ valorAtributo x (Right atributo)) > u ]
evaluarContinuo ejemplos _ atributo = ejemplos

-- either :: (Discreto -> [Ejemplo]) -> (Continuo -> [Ejemplo]) -> Atributo -> [Ejemplo]

evaluar :: [Ejemplo]  -> Atributo -> String -> [Ejemplo]
evaluar ejemplos atributo valor =
         either (evaluarDiscreto ejemplos valor) (evaluarContinuo ejemplos valor) atributo


valorAtributo :: Ejemplo -> Atributo -> ValorAtrib
valorAtributo ejemplo atributo =
       snd $ head (filter (\x -> fst x == atributo) (fst ejemplo))

