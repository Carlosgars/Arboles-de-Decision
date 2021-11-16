module AuxCART where

import TiposCART
import DiscretizarContinuo
import UtilsCART
import EjemplosCART
import Data.Either
import EjemplosCART
import Gini
import ECM


-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorclasifica :: ([Ejemplo] -> Atributo -> Double) -> [Atributo] -> [Ejemplo] -> Atributo
mejorclasifica f atributos ejemplos =
       let atributo_inic = head atributos
           valor_inic = f ejemplos atributo_inic 
       in mejorclasificaaux f (tail atributos) ejemplos atributo_inic valor_inic

mejorclasificaaux :: ([Ejemplo] -> Atributo -> Double) -> [Atributo] -> [Ejemplo] -> Atributo -> Double -> Atributo
mejorclasificaaux _ [] _ ac _ = ac
mejorclasificaaux f (atributo:atributos) ejemplos ac valor =
       let valor_new = f ejemplos atributo
       in if valor_new < valor
          then mejorclasificaaux f atributos ejemplos atributo valor_new
       else mejorclasificaaux f atributos ejemplos ac valor

mejorclasificaECM = mejorclasifica ecm_atributo
mejorclasificaGini = mejorclasifica gini_atributo

-- Criterios de parada

-- Comprobar si una lista de ejemplos es homogénea.

homogeneo :: [Ejemplo] -> (Bool, ValorAtrib)
homogeneo [] = (False, Left "Vacío")
homogeneo ejemplos =
          let clasificaciones = map clasificacion ejemplos
              hoja = head clasificaciones
          in if all (== hoja) (tail clasificaciones)
          then (True, hoja)
          else (False, hoja)

-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones que tengan el mismo número de ejemplos.

mascomun :: [Ejemplo] -> String
mascomun [] = "Aqui esta el error: mascomun de lista vacia"
mascomun ejemplos =
         let valores_clasificacion = (posiblesval.atributoObjetivo.head) ejemplos
             clasificaciones = map (getL.clasificacion) ejemplos
         in (maximo [ (x,ocurrencia clasificaciones x) | x <- valores_clasificacion ] (head clasificaciones,0))


paradaClasificacion :: Double -> [Ejemplo] -> (Bool, String)
paradaClasificacion min ejemplos =
       let n = fromIntegral $ length ejemplos
           h = mascomun ejemplos
           p_h = (fromIntegral $ ocurrencia (map (getL.clasificacion) ejemplos) h) / n
       in if p_h >= min then (True, h) else (False, h)

paradaRegresion :: Double -> [Ejemplo] -> (Bool, Double)
paradaRegresion max ejemplos =
       let n = fromIntegral $ length ejemplos
           pred = prediccionhoja ejemplos
           error = ecm ejemplos
       in if error <= max then (True, pred) else (False, pred)