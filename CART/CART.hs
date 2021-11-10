module CART where

import TiposCART
import Gini
import ECM
import EjemplosCART
import AuxCART
import DiscretizarContinuo
import UtilsCART

-- Clasificacion --

runCARTclasificacion = cart_clasificacion "error: Ejemplos vacio."

cart_clasificacion :: String -> [Atributo] -> [Ejemplo] -> Arbol
cart_clasificacion comunpadre _ [] = Hoja (Left comunpadre)
cart_clasificacion _ [] ejemplos = Hoja (Left (mascomun ejemplos))
cart_clasificacion comunpadre atributos ejemplos =
    let discretizados = map (discretizarGini ejemplos) atributos
        mejoratributo = mejorclasificaGini discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaClasificacion 0.75 ejemplos
        comunpadre = mascomun ejemplos
    in
    if fst posiblehoja
    then Hoja (Left (snd posiblehoja))
    else Nodo mejoratributo (creahijoclasificacion comunpadre new_atributos (evaluar ejemplos mejoratributo))


creahijoclasificacion :: String -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creahijoclasificacion comunpadre atributos mejorclasificasegunvalor valor =
         let ejemplos = mejorclasificasegunvalor valor
         in cart_clasificacion comunpadre atributos ejemplos


ranCARTclasificacion = runCARTclasificacion atLluviaCART ejemplosLluviaD

-- Regresion --

runCARTregresion = cart_regresion 0.0

cart_regresion :: Double -> [Atributo] -> [Ejemplo] -> Arbol
cart_regresion predpadre _ [] = Hoja (Right predpadre)
cart_regresion _ [] ejemplos = Hoja (Right (prediccionhoja ejemplos))
cart_regresion predpadre atributos ejemplos =
    let discretizados = map (discretizarECM ejemplos) atributos
        mejoratributo = mejorclasificaECM discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaRegresion 500 ejemplos
        predpadre = prediccionhoja ejemplos
    in
    if fst posiblehoja
    then Hoja (Right (snd posiblehoja))
    else Nodo mejoratributo (creahijoregresion predpadre new_atributos (evaluar ejemplos mejoratributo))


creahijoregresion :: Double -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creahijoregresion predpadre atributos mejorclasificasegunvalor valor =
         let ejemplos = mejorclasificasegunvalor valor
         in cart_regresion predpadre atributos ejemplos


ranCARTregresion = runCARTregresion atLluviaCART ejemplosLluviaC



cart :: String -> [Atributo] -> [Ejemplo] -> Arbol
cart "regresion" atributos ejemplos = runCARTregresion atributos ejemplos
cart "clasificacion" atributos ejemplos = runCARTclasificacion atributos ejemplos
cart _ atributos ejemplos = Hoja (Left "Seleccionar entre regresion o clasificacion")