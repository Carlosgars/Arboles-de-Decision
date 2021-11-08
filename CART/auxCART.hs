module AuxCART where

import TiposCART
import GananciaNormalizada
import DiscretizarContinuo
import UtilsCART
import EjemplosCART
import Data.Either
import EjemplosLluviaCART

-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorclasifica :: [Atributo] -> [Ejemplo] -> Atributo
mejorclasifica atributos ejemplos =
       let atributo_inic = head atributos
           ganancia_inic = ganancianorm ejemplos atributo_inic 
       in mejorclasificaaux (tail atributos) ejemplos atributo_inic ganancia_inic

mejorclasificaaux ::  [Atributo] -> [Ejemplo] -> Atributo -> Double -> Atributo
mejorclasificaaux [] _ ac _ = ac
mejorclasificaaux (atributo:atributos) ejemplos ac gan =
       let g_new = ganancianorm ejemplos atributo
       in if g_new > gan
          then mejorclasificaaux atributos ejemplos atributo g_new
       else mejorclasificaaux atributos ejemplos ac gan

-- version 2
-- ganancias :: [Atributo] -> [Ejemplo] -> [(Atributo,Double)]
-- ganancias atributos ejemplos =
--           map (\ x -> (x, ganancianorm ejemplos x)) atributos

-- mejorclasifica2 :: [Atributo] -> [Ejemplo] -> Atributo
-- mejorclasifica2 atributos ejemplos =
--         let maximo = maximum $ map snd ls
--             ls = (ganancias atributos ejemplos)
--         in fst $ head $ filter (\x -> snd x == maximo) ls


-- Criterios de parada

-- Comprobar si una lista de ejemplos es homogénea.

homogeneo :: [Ejemplo] -> (Bool, ValorAtrib)
homogeneo [] = (False, "Vacío")
homogeneo ejemplos =
          let clasificaciones = map clasificacion ejemplos
              hoja = head clasificaciones
          in if all (== hoja) (tail clasificaciones)
          then (True, hoja)
          else (False, hoja)



-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones que tengan el mismo número de ejemplos.

mascomun :: [Ejemplo] -> ValorAtrib
mascomun [] = Left "Aqui esta el error: mascomun de lista vacia"
mascomun ejemplos =
         let valores_clasificacion = (posiblesval.atributoObjetivo.head) ejemplos
             clasificaciones = map (clasificacion) ejemplos
         in maximo [ (x,ocurrencia x clasificaciones) | x <- valores_clasificacion ] (head clasificaciones,0)


parada :: Double -> [Ejemplo] -> (Bool, String)
parada min ejemplos =
       let n = fromIntegral $ length ejemplos
           h = mascomun ejemplos
           p_h = (fromIntegral $ ocurrencia h (map (getL.clasificacion) ejemplos)) / n
       in if p_h >= min then (True, h) else (False, h)