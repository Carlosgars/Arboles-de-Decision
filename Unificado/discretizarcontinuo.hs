module DiscretizarContinuo where

import Tipos
import Data.Either
import Data.List
import Utils
import Data.Maybe
import Gini
import ECM
import GananciaNormalizada

posiblesParticiones :: Continuo -> [Double] -> [Double]
posiblesParticiones atributo valores =
        let xs = ordensindups ([fst $ rango atributo] ++ valores ++ [snd $ rango atributo])
        in [ (x + y) / 2 | (x,y) <- zip xs (tail xs) ]

discretizar :: ([Ejemplo] -> Continuo -> Double -> Double) -> String -> [Ejemplo] -> Atributo -> Atributo
discretizar _ _ _ (Left atributo) = (Left atributo)
discretizar f maxmin ejemplos (Right atributo) =
        let umbral = mejorUmbral f maxmin atributo ejemplos
        in Right (C (nombreC atributo) (rango atributo) (Just umbral))

mejorUmbral :: ([Ejemplo] -> Continuo -> Double -> Double) -> String -> Continuo -> [Ejemplo] -> Double
mejorUmbral f maxmin atributo ejemplos =
        let v = ordensindups $ map getR (valores (Right atributo) ejemplos)
            us =  posiblesParticiones atributo v
            um_inic = head us
            gan_inic = f ejemplos atributo um_inic 
        in mejorUmbralAux f maxmin atributo ejemplos (tail us) um_inic gan_inic

mejorUmbralAux :: ([Ejemplo] -> Continuo -> Double -> Double) -> String -> Continuo -> [Ejemplo] -> [Double] -> Double -> Double -> Double
mejorUmbralAux _ _ _ _ [] umbral _ = umbral
mejorUmbralAux f "min" atributo ejemplos (u:us) umbral valor_f =
        let new_valor_f = f ejemplos atributo umbral
        in if new_valor_f < valor_f
        then mejorUmbralAux f "min" atributo ejemplos us u new_valor_f
        else mejorUmbralAux f "min" atributo ejemplos us umbral valor_f
mejorUmbralAux f "max" atributo ejemplos (u:us) umbral valor_f =
        let new_valor_f = f ejemplos atributo umbral
        in if new_valor_f > valor_f
        then mejorUmbralAux f "max" atributo ejemplos us u new_valor_f
        else mejorUmbralAux f "max" atributo ejemplos us umbral valor_f


discretizarGanInfo = discretizar gananciaNormCUmbral "max"
discretizarECM = discretizar ecmAtributoUmbral "min"
discretizarGini = discretizar giniAtributoUmbral "min"
