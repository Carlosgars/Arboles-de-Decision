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

discretizar :: ([Ejemplo] -> Continuo -> Double -> Double) -> (Double -> Double -> Bool) -> [Ejemplo] -> Atributo -> Atributo
discretizar _ _ _ (Left atributo) = (Left atributo)
discretizar f operador ejemplos (Right atributo) =
    let umbral = mejorUmbral f operador atributo ejemplos
    in Right (C (nombreC atributo) (rango atributo) (Just umbral))

mejorUmbral :: ([Ejemplo] -> Continuo -> Double -> Double) -> (Double -> Double -> Bool) -> Continuo -> [Ejemplo] -> Double
mejorUmbral f operador atributo ejemplos =
    let v = ordensindups $ map getR (valores (Right atributo) ejemplos)
        us =  posiblesParticiones atributo v
        umbral_inic = head us
        gan_inic = f ejemplos atributo umbral_inic 
    in mejorUmbralAux f operador atributo ejemplos (tail us) umbral_inic gan_inic

mejorUmbralAux :: ([Ejemplo] -> Continuo -> Double -> Double) -> (Double -> Double -> Bool) -> Continuo -> [Ejemplo] -> [Double] -> Double -> Double -> Double
mejorUmbralAux _ _ _ _ [] umbral _ = umbral
mejorUmbralAux f operador atributo ejemplos (u:us) umbral valor_f =
    let nuevo_valor_f = f ejemplos atributo umbral
    in if nuevo_valor_f `operador` valor_f
    then mejorUmbralAux f operador atributo ejemplos us u nuevo_valor_f
    else mejorUmbralAux f operador atributo ejemplos us umbral valor_f


discretizarGanInfo = discretizar gananciaNormCUmbral (>)
discretizarECM = discretizar ecmAtributoUmbral (<)
discretizarGini = discretizar giniAtributoUmbral (<)
