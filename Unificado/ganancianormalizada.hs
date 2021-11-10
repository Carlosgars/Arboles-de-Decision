module GananciaNormalizada where

import Tipos
import Utils
import Entropia
import Data.Either
import Data.Maybe

-- Discreto

gananciaInformacionD ::[Ejemplo] -> Discreto -> Double
gananciaInformacionD [] _ = 0
gananciaInformacionD ejemplos atributo =
         let valores = posiblesvaloresD atributo
             ganancia = sum $ map (calculoGananciaD ejemplos atributo) valores
         in entropia ejemplos - ganancia
         
gananciaNormD ::[Ejemplo] -> Discreto -> Double
gananciaNormD [] _ = 0
gananciaNormD ejemplos atributo =
         let norm = normaD ejemplos atributo
             valores = posiblesvaloresD atributo
             ganancia = sum $ map (calculoGananciaD ejemplos atributo) valores
         in
         (entropia ejemplos - ganancia) / norm
         
calculoGananciaD :: [Ejemplo] -> Discreto -> String -> Double
calculoGananciaD [] atributo valor = 0
calculoGananciaD ejemplos atributo valor =
           let sv = evaluarD ejemplos valor atributo
           in (entropia sv) * (lengthDouble sv) / (lengthDouble ejemplos)

normaD :: [Ejemplo] -> Discreto -> Double
normaD [] _ = 1
normaD ejemplos atributo =
           let valores = posiblesvaloresD atributo
           in sum $ map (calculoNormaD ejemplos atributo) valores

calculoNormaD :: [Ejemplo] -> Discreto -> String -> Double
calculoNormaD [] atributo valor = 1
calculoNormaD ejemplos atributo valor =
        let n = lengthDouble ejemplos
            vc = lengthDouble $ evaluarD ejemplos valor atributo
            pc = vc / n
            output = - pc * (logBase (fromIntegral 2)  pc)
        in  - pc * (logBase (fromIntegral 2)  pc)
        

gananciaNormCUmbral :: [Ejemplo] -> Continuo -> Double -> Double
gananciaNormCUmbral [] _ _ = 0
gananciaNormCUmbral ejemplos atributo umbral =
        let atrib = Right atributo
            s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
            s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
            n = lengthDouble ejemplos
            norm = (calculoNormaC s1 n) + (calculoNormaC s2 n)
        in if norm /= 0
        then (entropia ejemplos - (calculoGananciaC s1 n) - (calculoGananciaC s2 n)) / norm
        else 0 

calculoGananciaC :: [Ejemplo] -> Double -> Double
calculoGananciaC _ 0 = 0
calculoGananciaC [] _ = 0
calculoGananciaC s n =
        let p = (fromIntegral $ length s) / n
        in (entropia s) * p

calculoNormaC :: [Ejemplo] -> Double -> Double
calculoNormaC _ 0 = 0
calculoNormaC [] _ = 0
calculoNormaC s n =
        let p = (lengthDouble s) / n
        in - p * (logBase (fromIntegral 2)  p) 

gananciaNormC ::  [Ejemplo] -> Continuo -> Double
gananciaNormC [] _ = 0
gananciaNormC ejemplos atributo =
          let u = fromJust $ umbral atributo
          in gananciaNormCUmbral ejemplos atributo u

gananciaNorm :: [Ejemplo] -> Atributo -> Double
gananciaNorm [] _ = 0
gananciaNorm ejemplos atributo = either (gananciaNormD ejemplos) (gananciaNormC ejemplos) atributo

