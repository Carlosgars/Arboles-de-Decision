module ECM where

import Tipos
import Utils
import Data.Maybe


ecm :: [Ejemplo] -> Double
ecm ejemplos =
    let v          = map (getR.valorObjetivo) ejemplos
        prediccion = media v
        ecms       = map (\ x -> (x-prediccion)^2) v
    in media ecms

ecmAtributo :: [Ejemplo] -> Atributo -> Double
ecmAtributo ejemplos atributo =
     ecmAtributoUmbral ejemplos (getR atributo) (fromJust $ umbral $ getR atributo)
     
ecmAtributoUmbral :: [Ejemplo] -> Continuo -> Double -> Double
ecmAtributoUmbral ejemplos atributo umbral =
     let atrib = Right atributo
         n = lengthDouble ejemplos
         s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
         p1 = lengthDouble s1 / n
         s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
         p2 = lengthDouble s2 / n
     in p1 * (ecm s1) + p2 * (ecm s2)

