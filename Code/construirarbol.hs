module ConstruirArbol where

import Tipos
import Gini
import ECM
import MejorAtributo
import DiscretizarContinuo
import Utils
import Parada

arbolGenerico :: ([Ejemplo] -> ValorAtrib) -> ([Ejemplo] -> Atributo -> Atributo) -> ([Atributo] -> [Ejemplo] -> Atributo) -> (Double -> [Ejemplo] -> (Bool, ValorAtrib)) -> ValorAtrib -> Double -> [Atributo] -> [Ejemplo] -> Arbol
arbolGenerico _ _ _ _ hoja_padre _ _ [] = Hoja $ hoja_padre
arbolGenerico crea_hoja _ _ _ _ _ [] ejemplos = Hoja $ crea_hoja ejemplos
arbolGenerico crea_hoja discretizador mejorClasificador condicionParada hoja_padre param_parada atributos ejemplos =
    let discretizados    = map (discretizador ejemplos) atributos
        mejor_atributo   = mejorClasificador discretizados ejemplos
        nuevo_atributos  = elimina mejor_atributo atributos
        parada           = condicionParada param_parada ejemplos
        nueva_hoja_padre = crea_hoja ejemplos
    in
    if fst parada
    then Hoja $ snd parada
    else Nodo mejor_atributo
    (creaHijo crea_hoja discretizador mejorClasificador condicionParada
    nueva_hoja_padre param_parada nuevo_atributos (evaluar ejemplos mejor_atributo))

creaHijo :: ([Ejemplo] -> ValorAtrib) -> ([Ejemplo] -> Atributo -> Atributo) -> ([Atributo] -> [Ejemplo] -> Atributo) -> (Double -> [Ejemplo]-> (Bool, ValorAtrib)) -> ValorAtrib -> Double -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol     
creaHijo crea_hoja discretizador mejorClasificador condicionParada hoja_padre param_parada atributos evaluarValor valor =
    let ejemplos = evaluarValor valor
    in arbolGenerico crea_hoja discretizador mejorClasificador
    condicionParada hoja_padre param_parada atributos ejemplos

arbolC45 = arbolGenerico masComun discretizarGanInfo mejorClasificaGan paradaClasificacion
arbolCARTclas = arbolGenerico masComun discretizarGini mejorClasificaGini paradaClasificacion
arbolCARTreg = arbolGenerico prediccionHoja discretizarECM mejorClasificaECM paradaRegresion

c45 = arbolC45 $ Left "error: Ejemplos vacio."
cart "clasificacion" = arbolCARTclas $ Left "error: Ejemplos vacio."
cart "regresion" = arbolCARTreg $ Right 0.0