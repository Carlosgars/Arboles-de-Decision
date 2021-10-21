module AuxID3 where

import Tipos
import Utils
import Entropia

-- Comprobar si una lista de ejemplos es homogénea.

homogeneo :: (Eq b) => [Ejemplo a b] -> (Bool, b)
homogeneo ejemplos = 
          if all (== head etiqueta_ejemplos) (tail etiqueta_ejemplos)
          then (True,head etiqueta_ejemplos)
          else (False,head etiqueta_ejemplos)
          where etiqueta_ejemplos = map etiqueta ejemplos


-- Devolver etiqueta más común en lista de ejemplos.
-- Problema: dos clasificaciones quue tengan el mismo número de ejemplos.

mascomun :: (Eq b) => [Ejemplo a b] -> b
mascomun ejemplos =
          mascomunaux (map snd (map snd ejemplos)) ((posiblesvalores.fst.snd.head) ejemplos)

mascomunaux :: (Eq b) => [b] -> [b] -> b
mascomunaux ejemplos posiblesval =
              maximo [ (x,ocurrencia x ejemplos) | x <- posiblesval ] (head ejemplos,0)


--Nos quedamos con los ejemplos cuyo atributo tome un valor dado

atributoevaluado :: (Eq a) => [Ejemplo a b] -> Atributo a -> a -> [Ejemplo a b]
atributoevaluado ejemplos atributo valor =
                 [x | x <- ejemplos, (valoratributo x atributo) == valor ]

valoratributo :: (Eq a) => Ejemplo a b -> Atributo a -> a
valoratributo ejemplo atributo =
             snd$head (filter (\x -> fst x == atributo) (fst ejemplo))

-- Calculamos la ganancia de información de un atributo en un conjunto de ejemplos

gananciainformacion :: (Eq a) => (Eq b) => [Ejemplo a b] -> Atributo a -> Double
gananciainformacion ejemplos atributo =
                    (entropia ejemplos) - ganinfoaux ejemplos atributo (posiblesvalores atributo)

ganinfoaux :: (Eq a) => (Eq b) => [Ejemplo a b] -> Atributo a -> [a] -> Double
ganinfoaux ejemplos atributo [] = 0
ganinfoaux ejemplos atributo (v:posiblesvalores) =
           let sv = atributoevaluado ejemplos atributo v
           in
           (entropia sv) * (fromIntegral (length sv)) / (fromIntegral (length ejemplos))
           + (ganinfoaux ejemplos atributo posiblesvalores)

           

-- Encontrar atributo que mejor clasifica una lista de ejemplos.

mejorclasifica ::  (Eq a) => (Eq b) => [Atributo a] -> [Ejemplo a b] -> Atributo a
mejorclasifica atributos ejemplos = mcaux atributos ejemplos (head atributos)

mcaux ::  (Eq a) => (Eq b) => [Atributo a] -> [Ejemplo a b] -> Atributo a -> Atributo a
mcaux [] ejemplos ac = ac
mcaux (atributo:atributos) ejemplos ac =
       let g_current = gananciainformacion ejemplos ac
           g_next = gananciainformacion ejemplos atributo
       in
       if g_next > g_current
       then mcaux atributos ejemplos atributo
       else mcaux atributos ejemplos ac

