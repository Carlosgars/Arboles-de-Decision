module Error where

import Tipos
import Evaluar
import Utils


bienClasificado :: Arbol -> Ejemplo -> Bool
bienClasificado arbol ejemplo =
   let prediccion = predice arbol (fst ejemplo)
       clas_correcta = valorObjetivo ejemplo
   in prediccion == clas_correcta

errorClasificacionConjunto :: Arbol -> [Ejemplo] -> Double
errorClasificacionConjunto arbol ejemplos =
    let errores = lengthDouble $ filter (==False) (map (bienClasificado arbol) ejemplos)
        n = lengthDouble ejemplos
    in errores / n

errorRegresion :: Arbol -> Ejemplo -> Double
errorRegresion arbol ejemplo =
   let correcta = getR $ valorObjetivo ejemplo
       prediccion = getR $ predice arbol (fst ejemplo)
   in (correcta - prediccion) ^ 2

errorRegresionConjunto :: Arbol -> [Ejemplo] -> Double
errorRegresionConjunto arbol ejemplos =
    let error = sum $ map (errorRegresion arbol) ejemplos
        n = lengthDouble ejemplos
    in error / n

