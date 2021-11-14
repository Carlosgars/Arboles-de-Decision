module RunCARTonMarks where

import System.IO.Unsafe
import System.IO
import ReadCSV
import Prepare
import Tipos
import ConstruirArbol
import MejorAtributo
import GananciaNormalizada
import Error
import RandomForest

dataset :: [Ejemplo]
dataset = unsafePerformIO $ prepare 500

training_validation :: ([Ejemplo],[Ejemplo])
training_validation = splitAt 450 dataset

training = fst training_validation
validation = snd training_validation

atributos = [Right genderC,Right raceC,Right parentsedC,Right lunchC,Right readingscore,Right writingscore, Right courseC]
atObjetivo = Right mathscore

arbol = cart "regresion" 150 atributos training

mc = mejorClasificaGan atributos training
gan = map (gananciaNorm training) (atributos)

errorArbolEntrenamiento = errorRegresionConjunto arbol training
errorArbolValidacion = errorRegresionConjunto arbol (take 10 validation)



boostedentrenamientos = bootsEjemplos 10 training (5,5)
boostedatributos = bootsAtribs 10 5 atributos (4,3)

rf = (construirkArboles 50 training atributos 3 (cart "regresion" 150))

errorRFentrenamiento = errorRFreg rf training
errorRFvalidacion = errorRFreg rf validation

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