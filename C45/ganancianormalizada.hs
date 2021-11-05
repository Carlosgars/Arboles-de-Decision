module GananciaNormalizada where

import TiposC45
import Data.Either
import UtilsC45
import Data.Maybe
import EjemplosC45

--- Entropia ---
entropia :: [Ejemplo] -> Double
entropia [] = 0
entropia ejemplos =
          let valores = posiblesvalores $ getL $ atributoObjetivo $ head ejemplos
          in sum $ map (calculoentropia ejemplos) valores
          
calculoentropia :: [Ejemplo] -> String -> Double
calculoentropia ejemplos valor =
            let n = fromIntegral $ length ejemplos
                frec = ocurrencia valor (map (getL.snd.snd) ejemplos)
                p = fromIntegral (frec) / n
            in
            if p > 0 then - p * (logBase (fromIntegral 2)  p)
            else 0

--- Ganancia de Información ---

-- Discreto


ganancianormD ::[Ejemplo] -> Discreto -> Double
ganancianormD [] _ = 0
ganancianormD ejemplos atributo =
         let norm = normaD ejemplos atributo
             valores = posiblesvalores atributo
             ganancia = sum $ map (calculoGanD ejemplos atributo) valores
         in
         (entropia ejemplos - ganancia) / norm
         
calculoGanD :: [Ejemplo] -> Discreto -> String -> Double
calculoGanD ejemplos atributo valor =
           let sv = evaluarDiscreto ejemplos valor atributo
           in
           (entropia sv) * (fromIntegral $ length sv) / (fromIntegral $ length ejemplos)

normaD :: [Ejemplo] -> Discreto -> Double
normaD [] _ = 0
normaD ejemplos atributo =
           let valores = posiblesvalores atributo
           in sum $ map (calculonorm ejemplos atributo) valores

calculonorm :: [Ejemplo] -> Discreto -> String -> Double
calculonorm ejemplos atributo valor =
        let n = fromIntegral $ length ejemplos
            vc = fromIntegral $ length $ evaluarDiscreto ejemplos valor atributo
            pc = vc / n
        in - pc * (logBase (fromIntegral 2)  pc)
        

ganInfoNormCUmbral :: Continuo -> [Ejemplo] -> Double -> Double
ganInfoNormCUmbral atributo ejemplos umbral =
        let atrib = Right atributo
            s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
            s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
            n = fromIntegral $ length ejemplos
            p1 = (fromIntegral $ length s1) / n
            p2 = (fromIntegral $ length s2) / n
            norm = - p1 * (logBase (fromIntegral 2)  p1) - p2 * (logBase (fromIntegral 2)  p2) 
        in (entropia ejemplos - (entropia s1) * p1 - (entropia s2) * p2) / norm

ganancianormC ::  [Ejemplo] -> Continuo -> Double
ganancianormC ejemplos atributo =
          let u = fromJust $ umbral atributo
          in ganInfoNormCUmbral atributo ejemplos u

ganancianorm :: [Ejemplo] -> Atributo -> Double
ganancianorm [] _ = 0
ganancianorm ejemplos atributo = either (ganancianormD ejemplos) (ganancianormC ejemplos) atributo

