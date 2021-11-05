module AuxC45 where

import TiposC45
import GananciaNormalizada
import DiscretizarContinuo
import UtilsC45
import EjemplosC45
import Data.Either



-- Comprobar si una lista de ejemplos es homogénea.

homogeneo :: [Ejemplo] -> (Bool, String)
homogeneo [] = (False, "nana")
homogeneo ejemplos = 
          if all (== head etiqueta_ejemplos) (tail etiqueta_ejemplos)
          then (True,getL $ head etiqueta_ejemplos)
          else (False,getL $ head etiqueta_ejemplos)
          where etiqueta_ejemplos = map clasificacion ejemplos

-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones que tengan el mismo número de ejemplos.

mascomun :: [Ejemplo] -> String
mascomun ejemplos =
          mascomunaux (lefts $ map snd $ map snd ejemplos) ((posiblesval.atributoObjetivo.head) ejemplos)

mascomunaux :: [String] -> [String] -> String
mascomunaux ejemplos val =
              maximo [ (x,ocurrencia x ejemplos) | x <- val ] (head ejemplos,0)


-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorclasifica :: [Atributo] -> [Ejemplo] -> Atributo
mejorclasifica atributos ejemplos =
       mcaux (tail atributos) ejemplos (head atributos) 0

mcaux ::  [Atributo] -> [Ejemplo] -> Atributo -> Double -> Atributo 
mcaux [] ejemplos ac _ = ac
mcaux (atributo:atributos) ejemplos ac gmax =
       let gnew = ganancianorm ejemplos atributo
       in
       if gnew >= gmax
       then mcaux atributos ejemplos atributo gnew
       else mcaux atributos ejemplos ac gmax

-- version 2
ganancias :: [Atributo] -> [Ejemplo] -> [(Atributo,Double)]
ganancias atributos ejemplos =
          map (\ x -> (x, ganancianorm ejemplos x)) atributos

mejorclasifica2 :: [Atributo] -> [Ejemplo] -> Atributo
mejorclasifica2 atributos ejemplos =
        let maximo = maximum $ map snd ls
            ls = (ganancias atributos ejemplos)
        in fst $ head $ filter (\x -> snd x == maximo) ls