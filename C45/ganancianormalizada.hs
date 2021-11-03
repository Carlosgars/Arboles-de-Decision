module GananciaNormalizada where

import TiposC45
import Data.Either
import UtilsC45
import Data.Maybe


ganInfNormD ::[Ejemplo] -> Discreto -> Double
ganInfNormD ejemplos atributo =
         let norm = normD ejemplos atributo
         in
         ((entropia ejemplos)
         - ganInfoDaux ejemplos atributo (posiblesvalores $ atributo)) / norm

ganInfoDaux :: [Ejemplo] -> Discreto -> [String] -> Double
ganInfoDaux ejemplos atributo [] = 0
ganInfoDaux ejemplos atributo (v:posiblesvalores) =
           let sv = evaluarDiscreto ejemplos v atributo
           in
           (entropia sv) * (fromIntegral (length sv)) / (fromIntegral (length ejemplos))
           + (ganInfoDaux ejemplos atributo posiblesvalores)

normD :: [Ejemplo] -> Discreto -> Double
normD ejemplos atributo =
           normAux ejemplos atributo (posiblesvalores atributo) 0
           
normAux :: [Ejemplo] -> Discreto -> [String] -> Double -> Double
normAux ejemplos atributo [] ac = ac
normAux ejemplos atributo (c:valores) ac =
        let n = fromIntegral $ length ejemplos
            vc = fromIntegral $ (length $ evaluarDiscreto ejemplos c atributo)
            pc = vc / n
            new_ac = ac - pc * (logBase (fromIntegral 2)  pc)
        in normAux ejemplos atributo valores new_ac


ganInfoNormCUmbral :: Continuo -> [Ejemplo] -> Double -> Double
ganInfoNormCUmbral atributo ejemplos umbral =
        let s1 = [ x | x <- ejemplos,
                    (getR $ valorAtributo x (Right atributo)) <= umbral ]
            s2 = [ x | x <- ejemplos,
                    (getR $ valorAtributo x (Right atributo)) > umbral ]
            n = fromIntegral $ length ejemplos
            p1 = (fromIntegral $ length s1) / n
            p2 = (fromIntegral $ length s2) / n
            norm = - p1 * (logBase (fromIntegral 2)  p1) - p2 * (logBase (fromIntegral 2)  p2)
        in (entropia ejemplos - (entropia s1) * p1 - (entropia s2) * p2) / norm

ganInfoNormC ::  [Ejemplo] -> Continuo -> Double
ganInfoNormC ejemplos atributo =
          let u = fromJust $ umbral atributo
          in ganInfoNormCUmbral atributo ejemplos u

ganInfoNorm :: [Ejemplo] -> Atributo -> Double
ganInfoNorm ejemplos atributo = ( either (ganInfNormD ejemplos) (ganInfoNormC ejemplos) atributo )

