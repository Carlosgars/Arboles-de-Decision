module ConstruirModelos where

import Tipos
import Gini
import ECM
import MejorAtributo
import DiscretizarContinuo
import Utils
import Parada

----------
-- C4.5 --
----------

c45 = c45Aux $ Left "error: Ejemplos vacio."

c45Aux :: ValorAtrib -> Double -> [Atributo] -> [Ejemplo] -> Arbol
c45Aux mas_comun_padre _ _ [] = Hoja $ mas_comun_padre
c45Aux _ _ [] ejemplos = Hoja $ masComun ejemplos
c45Aux mas_comun_padre min_parada atributos ejemplos =
    let discretizados = map (discretizarGanInfo ejemplos) atributos
        mejor_atributo = mejorClasificaGan discretizados ejemplos
        nuevo_atributos = elimina mejor_atributo atributos
        parada = paradaClasificacion min_parada ejemplos
        nuevo_mas_comun_padre = masComun ejemplos
    in
    if fst parada
    then Hoja $ snd parada
    else Nodo mejor_atributo
    (creaHijoC45 nuevo_mas_comun_padre min_parada nuevo_atributos (evaluar ejemplos mejor_atributo))


creaHijoC45 :: ValorAtrib -> Double -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creaHijoC45 mas_comun_padre min_parada atributos evaluarValor valor =
         let ejemplos = evaluarValor valor
         in c45Aux mas_comun_padre min_parada atributos ejemplos

----------
-- CART --
----------

cart :: String -> [Atributo] -> [Ejemplo] -> Arbol
cart "regresion" atributos ejemplos = runCARTregresion atributos ejemplos
cart "clasificacion" atributos ejemplos = cartClasificacion 0.75 atributos ejemplos
cart _ atributos ejemplos = Hoja $ Left "error: Seleccionar entre regresion o clasificacion"

-- Clasificacion --

cartClasificacion = cartClasificacionAux $ Left "error: Ejemplos vacio."

cartClasificacionAux :: ValorAtrib -> Double -> [Atributo] -> [Ejemplo] -> Arbol
cartClasificacionAux mas_comun_padre _ _ [] = Hoja mas_comun_padre
cartClasificacionAux _ _ [] ejemplos = Hoja $ masComun ejemplos
cartClasificacionAux mas_comun_padre min_parada atributos ejemplos =
    let discretizados = map (discretizarGini ejemplos) atributos
        mejor_atributo = mejorClasificaGini discretizados ejemplos
        nuevo_atributos = elimina mejor_atributo atributos
        posiblehoja = paradaClasificacion min_parada ejemplos
        nuevo_mas_comun_padre = masComun ejemplos
    in
    if fst posiblehoja
    then Hoja $ snd posiblehoja
    else Nodo mejor_atributo
    (creaHijoCARTclas nuevo_mas_comun_padre min_parada nuevo_atributos (evaluar ejemplos mejor_atributo))

creaHijoCARTclas :: ValorAtrib -> Double -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creaHijoCARTclas mas_comun_padre min_parada atributos evaluarValor valor =
         let ejemplos = evaluarValor valor
         in cartClasificacionAux mas_comun_padre min_parada atributos ejemplos



-- Regresion --

cartRegresion :: ValorAtrib -> [Atributo] -> [Ejemplo] -> Arbol
cartRegresion predpadre _ [] = Hoja predpadre
cartRegresion _ [] ejemplos = Hoja $ prediccionHoja ejemplos
cartRegresion predpadre atributos ejemplos =
    let discretizados = map (discretizarECM ejemplos) atributos
        mejoratributo = mejorClasificaECM discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = paradaRegresion 500 ejemplos
        predpadre = prediccionHoja ejemplos
    in
    if fst posiblehoja
    then Hoja $ snd posiblehoja
    else Nodo mejoratributo (creaHijoRegresion predpadre new_atributos (evaluar ejemplos mejoratributo))


creaHijoRegresion :: ValorAtrib -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creaHijoRegresion predpadre atributos evaluarvalor valor =
         let ejemplos = evaluarvalor valor
         in cartRegresion predpadre atributos ejemplos

runCARTregresion = cartRegresion $ Right 0.0










