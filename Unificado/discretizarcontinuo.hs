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
    let v           = ordensindups $ map getR (valores (Right atributo) ejemplos)
        us          =  posiblesParticiones atributo v
        umbral_inic = head us
        f_inic      = f ejemplos atributo umbral_inic
    in fst $ foldl (\ (u, f_u) x ->
                   let f_x = f ejemplos atributo x
                   in if f_x `operador` f_u
                      then (x, f_x)
                      else (u, f_u) )
             (head us,f_inic) (tail us)

discretizarGanInfo = discretizar gananciaNormCUmbral (>)
discretizarECM = discretizar ecmAtributoUmbral (<)
discretizarGini = discretizar giniAtributoUmbral (<)