module MejorAtributo where

import Tipos
import GananciaNormalizada
import DiscretizarContinuo
import Utils
import Gini
import ECM
import Data.Either

-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorClasifica :: ([Ejemplo] -> Atributo -> Double) -> String -> [Atributo] -> [Ejemplo] -> Atributo
mejorClasifica f minmax atributos ejemplos =
       let atributo_inic = head atributos
           valor_inic = f ejemplos atributo_inic 
       in mejorClasificaAux f minmax (tail atributos) ejemplos atributo_inic valor_inic

mejorClasificaAux :: ([Ejemplo] -> Atributo -> Double) -> String -> [Atributo] -> [Ejemplo] -> Atributo -> Double -> Atributo
mejorClasificaAux _ _ [] _ ac _ = ac
mejorClasificaAux f "min" (atributo:atributos) ejemplos ac valor =
       let valor_new = f ejemplos atributo
       in if valor_new < valor
          then mejorClasificaAux f "min" atributos ejemplos atributo valor_new
       else mejorClasificaAux f "min" atributos ejemplos ac valor
mejorClasificaAux f "max" (atributo:atributos) ejemplos ac valor =
       let valor_new = f ejemplos atributo
       in if valor_new > valor
          then mejorClasificaAux f "max" atributos ejemplos atributo valor_new
       else mejorClasificaAux f "max" atributos ejemplos ac valor

mejorClasificaGan = mejorClasifica gananciaNorm "max"
mejorClasificaECM = mejorClasifica ecmAtributo "min"
mejorClasificaGini = mejorClasifica giniAtributo "min"