module RandomForest where

import Tipos
import Utils
import System.Random
import ConstruirModelos
import Evaluar
import ECM
import Error

generadores n i j = map mkStdGen (take n [i,j..])

selecRandom :: [a] -> StdGen -> a
selecRandom xs generador = xs !! rand where
    n = length xs
    (rand, _) = randomR (0,(n-1)) generador

subListaRandom n i j xs =
    map (selecRandom xs) (generadores n i j)

bootsEjemplos :: Int -> [Ejemplo] -> (Int,Int) -> [ [Ejemplo] ]
bootsEjemplos 0 _ _ = []
bootsEjemplos k ejemplos (i,j) =
    let n = length ejemplos
    in (subListaRandom n i j ejemplos) : bootsEjemplos (k-1) ejemplos (3*i,2*j)

bootsAtribs :: Int -> Int -> [Atributo] -> (Int,Int) -> [ [Atributo] ]
bootsAtribs 0 _ _ _ = []
bootsAtribs _ _ [] _ = []
bootsAtribs k n atributos (i,j) =
    let randomAt = subListaRandom n i j atributos
    in randomAt : bootsAtribs (k-1) n atributos (3*i,2*j)
    
bootsAtribSinReemplazo :: Int -> Int -> [Atributo] -> (Int,Int) -> [ [Atributo] ]
bootsAtribSinReemplazo 0 _ _ _ = []
bootsAtribSinReemplazo _ _ [] _ = []
bootsAtribSinReemplazo k n atributos (i,j) =
    let randomAt = subListaRandom n i j atributos
        nuevosAt = eliminaLista randomAt atributos
    in randomAt : bootsAtribSinReemplazo (k-1) n nuevosAt (3*i,2*j)



construirkArboles :: Int -> [Ejemplo] -> [Atributo] -> Int -> ([Atributo] -> [Ejemplo] -> Arbol) -> [Arbol]
construirkArboles k ejemplos atributos n_atributos modelo =
   let n_e = length ejemplos
       n_a = length atributos
       k_ejemplos = bootsEjemplos k ejemplos (21,13)
       k_atrib = bootsAtribs k n_atributos atributos (13,7)
       zip_ej_at = zip k_atrib k_ejemplos
   in map (\ (at,ej) -> modelo at ej) zip_ej_at


listaPredicciones :: Ejemplo -> [Arbol] -> [ValorAtrib]
listaPredicciones ejemplo arboles =
    map (\ arbol -> predice arbol (fst ejemplo)) arboles


votoMayoritario :: (Eq a) => [a] -> (a,Double)
votoMayoritario xs =
    let n = lengthDouble xs
        m = maximo [ (x,ocurrencia xs x) | x <- xs ] (head xs,0)
        p = (ocurrencia xs m) / n
    in (m,p)

prediccionCombinada :: Ejemplo -> [Arbol] -> ValorAtrib
prediccionCombinada (at_val,(Left atOb,x)) arboles =
    fst $ votoMayoritario $ listaPredicciones (at_val,(Left atOb,x)) arboles
prediccionCombinada (at_val,(Right atOb,x)) arboles =
    Right (media $ map getR $ listaPredicciones (at_val,(Right atOb,x)) arboles)


errorRFclas :: [Arbol] -> [Ejemplo] -> Double
errorRFclas arboles ejemplos =
    let predicciones      = map (\ x -> prediccionCombinada x arboles) ejemplos
        correctas         = map valorObjetivo ejemplos  
        bien_clasificados = zipWith (==) predicciones correctas
        errores           = lengthDouble $ filter (==False) bien_clasificados
        n                 = lengthDouble ejemplos
    in errores / n

errorRFreg :: [Arbol] -> [Ejemplo] -> Double
errorRFreg arboles ejemplos =
    let predicciones      = map (\ x -> getR $ prediccionCombinada x arboles) ejemplos
        correctas         = map (getR.valorObjetivo) ejemplos  
        diferencias       = zipWith (-) predicciones correctas
        ecm               = sum $ map (^2) diferencias
        n                 = lengthDouble ejemplos
    in ecm / n

filtrarBosque :: (Arbol -> [Ejemplo] -> Double) -> [Arbol] -> [Ejemplo] -> Double -> [Arbol]
filtrarBosque f arboles ejemplos umbral =
       foldl (\ac arbol -> if (f arbol ejemplos) < umbral
              then arbol:ac else ac) [] arboles
