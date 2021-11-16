module Parada where

import Tipos
import Utils
import ECM

masComun :: [Ejemplo] -> ValorAtrib
masComun [] = Left "error: mascomun de lista vacia"
masComun ejemplos =
    let valores_clasificacion = (posiblesValores.atributoObjetivo.head) ejemplos
        clasificaciones       = map (getL.valorObjetivo) ejemplos
    in Left $ maximo [ (x,ocurrencia clasificaciones x) | x <- valores_clasificacion ]
    (head clasificaciones,0)

paradaClasificacion :: Double -> [Ejemplo] -> (Bool, ValorAtrib)
paradaClasificacion min ejemplos =
    let n   = lengthDouble ejemplos
        h   = masComun ejemplos
        p_h = (ocurrencia (map valorObjetivo ejemplos) h) / n
    in if p_h >= min
       then (True, h)
       else (False, h)

prediccionHoja :: [Ejemplo] -> ValorAtrib
prediccionHoja ejemplos =
    let v = map (getR.valorObjetivo) ejemplos
    in Right $ media v
    
paradaRegresion :: Double -> [Ejemplo] -> (Bool, ValorAtrib)
paradaRegresion max ejemplos =
    let n     = lengthDouble ejemplos
        pred  = prediccionHoja ejemplos
        error = ecm ejemplos
    in if error <= max
       then (True, pred)
       else (False, pred)