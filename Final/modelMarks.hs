module ModelMarks
    where

import System.IO.Unsafe
import ReadAndPrepareMarks
import Tipos
import ConstruirArbol
import Error
import RandomForest

-- Preparar ejemplos

ejemplos :: [Ejemplo]
ejemplos = unsafePerformIO $ prepare 500 "students_performance.csv" prepareItem

splitEjemplos = splitAt 450 ejemplos :: ([Ejemplo],[Ejemplo])

entrenamiento = fst splitEjemplos

validacion    = snd splitEjemplos

-- Atributos

atributos  = [Right gender,Right race,Right parentsed,Right lunch,Right course,Right readingscore,Right writingscore]

atObjetivo = Right mathscore

-- Construir árbol

param_parada = 200

arbol = cart "regresion" param_parada atributos entrenamiento

-- Random Forest

n_arboles   = 20
n_atributos = 3
param_parada_bosque = 150

prebosque = (construirkArboles n_arboles entrenamiento atributos n_atributos (cart "regresion" param_parada_bosque))

bosque = filtrarBosque errorRegresionConjunto prebosque entrenamiento 400


-- Evaluamos los errores

errorArbolEntrenamiento = errorRegresionConjunto arbol entrenamiento
errorArbolValidacion    = errorRegresionConjunto arbol validacion

errorBosqueEntrenamiento = errorRFreg bosque entrenamiento
errorBosqueValidacion    = errorRFreg bosque validacion
