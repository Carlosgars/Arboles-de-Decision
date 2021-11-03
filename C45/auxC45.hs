module AuxC45 where

import TiposC45
import GananciaNormalizada
import DiscretizarContinuo
import UtilsC45
import EjemplosC45
import Data.Either



-- Comprobar si una lista de ejemplos es homogénea.

homogeneo :: [Ejemplo] -> (Bool, String)
homogeneo ejemplos = 
          if all (== head etiqueta_ejemplos) (tail etiqueta_ejemplos)
          then (True,getL $ head etiqueta_ejemplos)
          else (False,getL $ head etiqueta_ejemplos)
          where etiqueta_ejemplos = map clasificacion ejemplos

-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones que tengan el mismo número de ejemplos.

mascomun :: [Ejemplo] -> String
mascomun ejemplos =
          mascomunaux (lefts $ map snd $ map snd ejemplos) ((posiblesvalores.getL.atributoObjetivo.head) ejemplos)

mascomunaux :: [String] -> [String] -> String
mascomunaux ejemplos posiblesval =
              maximo [ (x,ocurrencia x ejemplos) | x <- posiblesval ] (head ejemplos,0)


-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorclasifica :: [Atributo] -> [Ejemplo] -> Atributo
mejorclasifica atributos ejemplos = mcaux (tail atributos) ejemplos (head atributos)

mcaux ::  [Atributo] -> [Ejemplo] -> Atributo -> Atributo 
mcaux [] ejemplos ac = ac
mcaux (atributo:atributos) ejemplos ac =
       let g_current = ganInfoNorm ejemplos ac
           g_next = ganInfoNorm ejemplos atributo
       in
       if g_next > g_current
       then mcaux atributos ejemplos atributo
       else mcaux atributos ejemplos ac