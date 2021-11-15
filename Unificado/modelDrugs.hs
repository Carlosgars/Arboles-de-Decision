module ModelDrugs
    where

import System.IO.Unsafe
import ReadandPrepareDrugs 
import Tipos
import ConstruirArbol
import Error
import RandomForest

-- Preparar ejemplos

ejemplos :: [Ejemplo]
ejemplos = unsafePerformIO $ prepare 201 "drug200.csv" prepareItem

splitEjemplos :: ([Ejemplo],[Ejemplo])
splitEjemplos = splitAt 150 ejemplos

entrenamiento = fst splitEjemplos :: [Ejemplo]

validacion    = snd splitEjemplos :: [Ejemplo]

-- Atributos

atributos  = [Right age,Left sex,Left bp,Left cholesterol,Right na_to_K]

atObjetivo = Left drug

-- Construir árbol

param_parada = 0.5

arbol = c45 param_parada atributos entrenamiento

-- Random Forest

n_arboles = 100
n_atributos = 4

bosque = (construirkArboles n_arboles entrenamiento atributos n_atributos (c45 param_parada))


-- Evaluar error

errorArbolEntrenamiento = errorClasificacionConjunto arbol entrenamiento
errorArbolValidacion    = errorClasificacionConjunto arbol validacion

errorRFentrenamiento = errorRFclas bosque entrenamiento
errorRFvalidacion    = errorRFclas bosque validacion

