module Error where

import Tipos
import Evaluar
import Utils


bienClasificado :: Arbol -> Ejemplo -> Bool
bienClasificado arbol ejemplo =
   let prediccion = predice arbol (fst ejemplo)
       clas_correcta = valorObjetivo ejemplo
   in prediccion == clas_correcta


-- devolver el porcentaje de ejemplos clasificados incorrectamente

errorClasificacionConjunto :: Arbol -> [Ejemplo] -> Double
errorClasificacionConjunto arbol ejemplos =
    let atribvalor = map fst ejemplos
        predicciones = map (predice arbol) atribvalor
        correctas = map (valorObjetivo) ejemplos
        pred_clas = zip predicciones correctas
        errores = sum $ map (\(x,y) -> if x == y then 0 else 1) pred_clas
        n = lengthDouble ejemplos
    in errores / n


errorRegresion :: Arbol -> Ejemplo -> Double
errorRegresion arbol ejemplo =
   let correcta = getR $ valorObjetivo ejemplo
       prediccion = getR $ predice arbol (fst ejemplo)
   in (correcta - prediccion) ^ 2

-- devolver el ECM de los ejemplos y sus predicciones
errorRegresionConjunto :: Arbol -> [Ejemplo] -> Double
errorRegresionConjunto arbol ejemplos =
    let error = sum $ map (errorRegresion arbol) ejemplos
        n = lengthDouble ejemplos
    in error / n

