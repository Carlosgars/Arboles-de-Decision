module GananciaNormalizada where

import TiposDiversos
import Data.Either
import DiscretizarContinuo

-- Utils

maximo :: [(b,Int)] -> (b,Int) -> b
maximo [] y = fst y
maximo (x:xs) y = if (snd x) > (snd y) then maximo xs x else maximo xs y

ocurrencia :: (Eq a) => a -> [a] -> Int
ocurrencia a [] = 0
ocurrencia a (x:xs) = if a == x then 1 + ocurrencia a xs else ocurrencia a xs


elimina _ []                 = []
elimina x (y:ys) | x == y    = elimina x ys
                    | otherwise = y : elimina x ys
---


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


-- Calculamos la ganancia de información de un atributo en un conjunto de ejemplos

ganInfoD ::[Ejemplo] -> Discreto -> Double
ganInfoD ejemplos atributo =
       (entropia ejemplos)
       - ganInfoDaux ejemplos atributo (posiblesvalores $ atributo)

ganInfoDaux :: [Ejemplo] -> Discreto -> [String] -> Double
ganInfoDaux ejemplos atributo [] = 0
ganInfoDaux ejemplos atributo (v:posiblesvalores) =
           let sv = evaluarDiscreto ejemplos v atributo
           in
           (entropia sv) * (fromIntegral (length sv)) / (fromIntegral (length ejemplos))
           + (ganInfoDaux ejemplos atributo posiblesvalores)

ganInfoCUmbral :: [Ejemplo] -> Double -> Continuo -> Double
ganInfoCUmbral ejemplos umbral atributo =
           (entropia ejemplos)
           - ganInfoCUmbralAux ejemplos atributo (posiblesValoresUmbral atributo umbral)

ganInfoCUmbralAux :: [Ejemplo] -> Continuo -> [String] -> Double
ganInfoCUmbralAux ejemplos atributo [] = 0
ganInfoCUmbralAux ejemplos atributo (v:posiblesvalores) =
           let sv = evaluarContinuo ejemplos v atributo
           in
           (entropia sv) * (fromIntegral (length sv)) / (fromIntegral (length ejemplos))
           + (ganInfoCUmbralAux ejemplos atributo posiblesvalores)


ganInfoC :: [Ejemplo] -> Continuo -> (Double,Double)
ganInfoC ejemplos atributo =
           let valoresejemplos = getContinuo $ valores (Right atributo) ejemplos 
               umbrales = rmdups $ posiblesParticiones atributo valoresejemplos
           in ganInfoCAux ejemplos umbrales atributo (head umbrales,-1)

ganInfoCAux:: [Ejemplo] -> [Double] -> Continuo -> (Double,Double) -> (Double,Double)
ganInfoCAux ejemplos [] atributo (umbral,ac) =
            (ac, umbral)
ganInfoCAux ejemplos (u:umbrales) atributo (umbral,ac) =
            let new = ganInfoCUmbral ejemplos u atributo
            in if new > ac
            then ganInfoCAux ejemplos umbrales atributo (u,new)
            else ganInfoCAux ejemplos umbrales atributo (umbral,ac)

ganInfo :: [Ejemplo] -> Atributo -> Double
ganInfo ejemplos atributo = either (ganInfoD ejemplos) (fst.ganInfoC ejemplos) atributo