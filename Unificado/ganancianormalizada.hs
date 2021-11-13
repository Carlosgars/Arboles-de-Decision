module GananciaNormalizada where

import Tipos
import Utils
import Entropia
import Data.Either
import Data.Maybe

gananciaInformacionD ::[Ejemplo] -> Discreto -> Double
gananciaInformacionD [] _ = 0
gananciaInformacionD ejemplos atributo =
    let val       = posiblesvaloresD atributo
        norm     = normaD ejemplos atributo
        ganancia = foldl (\ac x ->
            let sv = evaluarD ejemplos x atributo
            in ac + (entropia sv) * (lengthDouble sv) / (lengthDouble ejemplos)) 0 val
    in entropia ejemplos - ganancia

normaD :: [Ejemplo] -> Discreto -> Double
normaD [] _ = 1
normaD ejemplos atributo =
    let val = posiblesvaloresD atributo
        n  = lengthDouble ejemplos
    in foldl (\ac x -> let pc = (lengthDouble $ evaluarD ejemplos x atributo) / n
              in ac - pc * (logBase (fromIntegral 2)  pc)) 0 val
              
gananciaNormD ::[Ejemplo] -> Discreto -> Double
gananciaNormD [] _ = 0
gananciaNormD ejemplos atributo =
    gananciaInformacionD ejemplos atributo / normaD ejemplos atributo
                
gananciaNormCUmbral :: [Ejemplo] -> Continuo -> Double -> Double
gananciaNormCUmbral [] _ _ = 0
gananciaNormCUmbral ejemplos atributo umbral =
    let atrib = Right atributo
        s1   = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
        s2   = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
        n    = lengthDouble ejemplos
        (gan,norm) = foldl (\(g,n) s -> let p = (lengthDouble s) / n
                     in ( g + (entropia s) * p,
                         n - p * (logBase (fromIntegral 2)  p) ) )
                     (0,0) [s1,s2]
    in if norm /= 0
       then (entropia ejemplos - gan) / norm
       else 0 

gananciaNormC ::  [Ejemplo] -> Continuo -> Double
gananciaNormC [] _ = 0
gananciaNormC ejemplos atributo =
    let u = fromJust $ umbral atributo
    in gananciaNormCUmbral ejemplos atributo u

gananciaNorm :: [Ejemplo] -> Atributo -> Double
gananciaNorm [] _ = 0
gananciaNorm ejemplos atributo =
    either (gananciaNormD ejemplos) (gananciaNormC ejemplos) atributo
