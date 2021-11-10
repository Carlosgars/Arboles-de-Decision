module ConstruirModelos where

import Tipos
import Gini
import ECM
import MejorAtributo
import DiscretizarContinuo
import Utils
import Parada

---------
-- ID3 --
---------




----------
-- C4.5 --
----------

c45 = c45Aux "error: Ejemplos vacio."

c45Aux :: String -> [Atributo] -> [Ejemplo] -> Arbol
c45Aux mascomunpadre _ [] = Hoja (Left mascomunpadre)
c45Aux _ [] ejemplos = Hoja (Left (mascomun ejemplos))
c45Aux mascomunpadre atributos ejemplos =
    let discretizados = map (discretizarGanInfo ejemplos) atributos
        mejoratributo = mejorClasificaGan discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaClasificacion 0.75 ejemplos
        mascomunpadre = mascomun ejemplos
    in
    if fst posiblehoja
    then Hoja (Left (snd posiblehoja))
    else Nodo mejoratributo (creahijoc45 mascomunpadre new_atributos (evaluar ejemplos mejoratributo))


creahijoc45 :: String -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creahijoc45 mascomunpadre atributos evaluarvalor valor =
         let ejemplos = evaluarvalor valor
         in c45Aux mascomunpadre atributos ejemplos

----------
-- CART --
----------

cart :: String -> [Atributo] -> [Ejemplo] -> Arbol
cart "regresion" atributos ejemplos = runCARTregresion atributos ejemplos
cart "clasificacion" atributos ejemplos = runCARTclasificacion atributos ejemplos
cart _ atributos ejemplos = Hoja (Left "Seleccionar entre regresion o clasificacion")

-- Clasificacion --

cartClasificacion :: String -> [Atributo] -> [Ejemplo] -> Arbol
cartClasificacion mascomunpadre _ [] = Hoja (Left mascomunpadre)
cartClasificacion _ [] ejemplos = Hoja (Left (mascomun ejemplos))
cartClasificacion mascomunpadre atributos ejemplos =
    let discretizados = map (discretizarGini ejemplos) atributos
        mejoratributo = mejorClasificaGini discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaClasificacion 0.75 ejemplos
        mascomunpadre = mascomun ejemplos
    in
    if fst posiblehoja
    then Hoja (Left (snd posiblehoja))
    else Nodo mejoratributo (creaHijoCARTclas mascomunpadre new_atributos (evaluar ejemplos mejoratributo))

creaHijoCARTclas :: String -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creaHijoCARTclas mascomunpadre atributos evaluarvalor valor =
         let ejemplos = evaluarvalor valor
         in cartClasificacion mascomunpadre atributos ejemplos

runCARTclasificacion = cartClasificacion "error: Ejemplos vacio."

-- Regresion --

cartRegresion :: Double -> [Atributo] -> [Ejemplo] -> Arbol
cartRegresion predpadre _ [] = Hoja (Right predpadre)
cartRegresion _ [] ejemplos = Hoja (Right (prediccionHoja ejemplos))
cartRegresion predpadre atributos ejemplos =
    let discretizados = map (discretizarECM ejemplos) atributos
        mejoratributo = mejorClasificaECM discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaRegresion 500 ejemplos
        predpadre = prediccionHoja ejemplos
    in
    if fst posiblehoja
    then Hoja (Right (snd posiblehoja))
    else Nodo mejoratributo (creaHijoRegresion predpadre new_atributos (evaluar ejemplos mejoratributo))


creaHijoRegresion :: Double -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creaHijoRegresion predpadre atributos evaluarvalor valor =
         let ejemplos = evaluarvalor valor
         in cartRegresion predpadre atributos ejemplos

runCARTregresion = cartRegresion 0.0







