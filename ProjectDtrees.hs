
module ProjectDtrees where

import Data.Maybe (fromJust)
import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)




data Atributo a =
     A {aNombre :: String, rango :: [a]}
     deriving Show

data Instancia a=
     D {atributos :: [(Atributo a,a)]}
     deriving Show

type Etiquetado a b = (b,Instancia a)

data Arbol a b =
     Nodo {att :: Atributo a, hijo :: a -> (Arbol a b)}
     | Hoja b
     


instance (Show a,Show b) => Show (Arbol a b) where
         show d = showArbol d

showArbol :: (Show a, Show b) => (Arbol a b) -> String
showArbol (Hoja b) = show b
showArbol (Nodo att hijo) = 
          "\n" ++ (aNombre att) ++ "\n" ++ intercalate "   " [show a | a<-rango att] ++ "\n" ++
          intercalate "    " [ showArbol (hijo k) | k <- rango att] 

verArbol :: (Show a, Show b) => (Arbol a b) -> IO ()
verArbol a = putStrLn $ show a



indent :: Int -> String -> String
indent d = (replicate d ' ' ++)



f _ = (Hoja 10)
a = (Nodo {att = A {aNombre = "Edad", rango = [1,2]}, hijo = k})
k 1 = (Nodo {att = A {aNombre = "Altura", rango = [1,2]}, hijo = g})
k 2 = (Nodo {att = A {aNombre = "Peso", rango = [1,2]}, hijo = f})
att1 = A {aNombre = "Edad", rango = [1]}
att2 = A {aNombre = "Altura", rango = [1,2]}
att3 = A {aNombre = "Tension", rango = [2]}
att4 = A {aNombre = "Oido", rango = [1]}
g 1 = (Nodo {att = A {aNombre = "Tension", rango = [2]}, hijo = f})
g 2 = (Nodo {att = A {aNombre = "Oido", rango = [1]}, hijo = f})