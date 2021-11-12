module Parada where

import Tipos
import Utils
import ECM

homogeneo :: [Ejemplo] -> (Bool, ValorAtrib)
homogeneo [] = (False, Left "Vacío")
homogeneo ejemplos =
          let clasificaciones = map valorObjetivo ejemplos
              hoja = head clasificaciones
          in if all (== hoja) (tail clasificaciones)
          then (True, hoja)
          else (False, hoja)

-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones que tengan el mismo número de ejemplos.

mascomun :: [Ejemplo] -> String
mascomun [] = "error: mascomun de lista vacia"
mascomun ejemplos =
         let valores_clasificacion = (posiblesValores.atributoObjetivo.head) ejemplos
             clasificaciones = map (getL.valorObjetivo) ejemplos
         in (maximo [ (x,ocurrencia clasificaciones x) | x <- valores_clasificacion ] (head clasificaciones,0))


paradaClasificacion :: Double -> [Ejemplo] -> (Bool, String)
paradaClasificacion min ejemplos =
       let n = lengthDouble ejemplos
           h = mascomun ejemplos
           p_h = (fromIntegral $ ocurrencia (map (getL.valorObjetivo) ejemplos) h) / n
       in if p_h >= min then (True, h) else (False, h)

paradaRegresion :: Double -> [Ejemplo] -> (Bool, Double)
paradaRegresion max ejemplos =
       let n = lengthDouble ejemplos
           pred = prediccionHoja ejemplos
           error = ecm ejemplos
       in if error <= max then (True, pred) else (False, pred)