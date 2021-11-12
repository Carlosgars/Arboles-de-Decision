module RandomForest where

import Tipos
import Utils
import System.Random
import ConstruirModelos
import Evaluar
import ECM

generators n i j = map mkStdGen (take n [i,j..])

getRandomElement :: [a] -> StdGen -> a
getRandomElement xs generator = xs !! rand where
    n = length xs
    (rand, _) = randomR (0,(n-1)) generator

getRandomSubList n i j xs =
    map (getRandomElement xs) (generators n i j)

boostTrainings :: Int -> [Ejemplo] -> (Int,Int) -> [ [Ejemplo] ]
boostTrainings 0 _ _ = []
boostTrainings k ejemplos (i,j) =
    let n = length ejemplos
    in (getRandomSubList n i j ejemplos) : boostTrainings (k-1) ejemplos (3*i,2*j)


boostAtribs :: Int -> Int -> [Atributo] -> (Int,Int) -> [ [Atributo] ]
boostAtribs 0 _ _ _ = []
boostAtribs _ _ [] _ = []
boostAtribs k n atributos (i,j) =
    let randomAt = getRandomSubList n i j atributos
    in randomAt : boostAtribs (k-1) n atributos (3*i,2*j)
    
boostAtribSinReemplazo :: Int -> Int -> [Atributo] -> (Int,Int) -> [ [Atributo] ]
boostAtribSinReemplazo 0 _ _ _ = []
boostAtribSinReemplazo _ _ [] _ = []
boostAtribSinReemplazo k n atributos (i,j) =
    let randomAt = getRandomSubList n i j atributos
        nuevosAt = eliminaLista randomAt atributos
    in randomAt : boostAtribSinReemplazo (k-1) n nuevosAt (3*i,2*j)



buildkModels :: Int -> [Ejemplo] -> [Atributo] -> Int -> ([Atributo] -> [Ejemplo] -> Arbol) -> [Arbol]
buildkModels k ejemplos atributos n_atributos modelo =
   let n_e = length ejemplos
       n_a = length atributos
       k_ejemplos = boostTrainings k ejemplos (5,5)
       k_atrib = boostAtribs k n_atributos atributos (5,3)
       zip_ej_at = zip k_atrib k_ejemplos
   in map (\ (at,ej) -> modelo at ej) zip_ej_at


listaPredicciones :: Ejemplo -> [Arbol] -> [ValorAtrib]
listaPredicciones ejemplo arboles =
    map (\ arbol -> predice arbol (fst ejemplo)) arboles


votoMayoritario :: (Eq a) => [a] -> (a,Double)
votoMayoritario xs =
    let n = lengthDouble xs
        m = maximo [ (x,ocurrencia xs x) | x <- xs ] (head xs,0)
        p = (fromIntegral $ ocurrencia xs m) / n
    in (m,p)

prediccionCombinada :: Ejemplo -> [Arbol] -> ValorAtrib
prediccionCombinada (at_val,(Left atOb,x)) arboles =
    fst $ votoMayoritario $ listaPredicciones (at_val,(Left atOb,x)) arboles
prediccionCombinada (at_val,(Right atOb,x)) arboles =
    Right (media $ map getR $ listaPredicciones (at_val,(Right atOb,x)) arboles)
