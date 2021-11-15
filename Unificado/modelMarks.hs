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

param_parada = 150

arbol = cart "regresion" param_parada atributos entrenamiento

-- Random Forest

n_arboles   = 50
n_atributos = 3

bosque = (construirkArboles n_arboles entrenamiento atributos n_atributos (cart "regresion" param_parada))

-- Evaluamos los errores

errorArbolEntrenamiento = errorRegresionConjunto arbol entrenamiento
errorArbolValidacion    = errorRegresionConjunto arbol (take 10 validacion)

errorBosqueEtrenamiento = errorRFreg bosque entrenamiento
errorBosqueValidacion    = errorRFreg bosque validacion


-------RUN 1---------

--- DATASET 450 -- split en 300

----- Arbol: 0.75
----- Error = 0.44

----- RF: 20 training atributos 4 (c45 0.75)
-----Error 0.29

---------------------

-------RUN 2---------

--- DATASET 450 -- split en 300

----- Arbol: 0.75
--- Error:0.42

----- RF: 20 training atributos 4 (c45 0.7)
-----Error 0.32

---------------------

-------RUN 3---------

--- DATASET 450 -- split en 300

----- Arbol: 0.65
--- Error:0.42

----- RF: 30 training atributos 3 (c45 0.7)
-----Error 0.32