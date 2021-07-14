import Data.List (nub, maximumBy)
import Data.Function (on)
import Data.Tree


-- ALGORITMO ID3 (Ejemplos,Atributo_objetivo,Atributos)
-- Crea nodo raíz.
-- Si todos en Ejemplos son positivos, devuelve un árbol con un nodo raíz etiquetado con +.
-- Si todos en Ejemplos son negativos, devuelve un árbol con un nodo raíz etiquetado con -.
-- Si Atributos es vacío, devuelve un árbol con un nodo raíz, etiquetado con el valor del atributo_objetivo más común en Ejemplos.
-- En otro caso:
--    A <- atributo de Atributos que mejor clasifica Ejemplos.
--    Atributo para raíz del Árbol de Decisión = A
--    Para cada valor v_i del atributo A,
--         Añadir una nueva rama debajo del árbol correspondiente a A = v_i.
--         Sea Ejemplos(v_i) el conjunto de Ejemplos que tienen A = v_i.
--         Si Ejemplos(v_i) vacío:
--            Añadir en esta rama un nodo-hoja etiquetado con el valor del atributo_objetivo más común en Ejemplos.
--         En otro caso añadir en esta rama el subárbol ALGORITMO ID3 (Ejemplos(v_i),Atributo_objetivo,Atributos - {A})
-- End
-- Devolver raíz.


-- ¿Cómo estructuramos los ejemplos y atributos? ¿Tipos?
-- Ejemplos -> lista de Doubles, etiqueta del atributo objetivo
-- Atributos -> lista de nombres

-- ¿Cómo estructuramos el árbol?
-- Nodo | Rama

-- ¿Cómo encontramos el atributo que mejor clasifica unos ejemplos?
-- Necesitamos implementar:
-- Entropía del conjunto de ejemplos dado el atributo objetivo.
-- Ganancia de información dado un conjunto de ejemplos, el atributo objetivo, el atributo que separa los ejemplos.
--

-- TIPOS y DATOS --

type Ejemplo a b = ([(Atributo a,a)],(Atributo b,b))

data Atributo a =
  A { nombre :: String
       , posiblesvalores :: [a] }
       deriving Eq
       
instance Show (Atributo a) where
  show = nombre


data Arbol a b = Hoja b
     | Nodo { atrib::Atributo a, hijo::a -> (Arbol a b) }

instance (Show a, Show b) => Show (Arbol a b) where
    show x = showTree x ""


-- 
etiqueta :: Ejemplo a b -> b
etiqueta = snd.snd

atributoObjetivo :: (Ejemplo a b) -> (Atributo b)
atributoObjetivo  = fst.snd 

showTree :: (Show a, Show b) => (Arbol a b) -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.("|\n"++).showList [hijo a | a <- posiblesvalores atrib].('>':)



-- EJEMPLOS --

ejemplo1 = ([(altura,"alto"),(peso,"ligero")],(sexo,"hombre")) :: Ejemplo String String
ejemplo2 = ([(altura,"medio"),(peso,"pesado")],(sexo,"hombre")) :: Ejemplo String String
ejemplo3 = ([(altura,"bajo"),(peso,"ligero")],(sexo,"mujer")) :: Ejemplo String String

desconocido = ["alto","pesado"]

ejemplos = [ejemplo1,ejemplo2,ejemplo3] :: [Ejemplo String String]

sexo = A "genero" ["hombre","mujer"] :: Atributo String
altura = A "altura" ["bajo","medio","alto"] :: Atributo String
peso = A "peso" ["ligero", "pesado"] :: Atributo String

arbol = Nodo altura hijoaltura :: Arbol String String

hijoaltura :: String -> Arbol String String
hijoaltura "alto" = Nodo  peso hijopesoAlto
hijoaltura "medio" = Nodo peso hijopesoMedio
hijoaltura "bajo" = Hoja "mujer"

hijopesoAlto :: String -> Arbol String String
hijopesoAlto "ligero" = Hoja "hombre"
hijopesoAlto "pesado" = Hoja "hombre"

hijopesoMedio :: String -> Arbol String String
hijopesoMedio "ligero" = Hoja "mujer"
hijopesoMedio "pesado" = Hoja "hombre"

atributos = [altura,peso] :: [Atributo String]

--arbol = Nodo altura :: Arbol

id3 ::  [Atributo a] -> [Ejemplo a b] -> Arbol a b
id3 atributos ejemplos  = undefined
--    if fst (homogeneo ejemplos) then Hoja (snd (homogeneo ejemplos))
--    else if atributos == [] then Hoja (mas_comun ejemplos)
--    else 
--        Nodo mejor_atributo Hijo hijos
--    where
--        mejor_atributo = mejor_clasifica ejemplos
--        particion = dividir ejemplos mejor_atributo
--        hijos = map (id3 atributos) particion
        
        
-- Funciones necesarias para id3
homogeneo :: (Eq b) => [Ejemplo a b] -> (Bool, b)
homogeneo ejemplos = 
          if all (== head etiqueta_ejemplos) (tail etiqueta_ejemplos)
          then (True,head etiqueta_ejemplos)
          else (False,head etiqueta_ejemplos)
          where etiqueta_ejemplos = map etiqueta ejemplos

-- problema cuando dos clasificaciones tienen el mismo numero de ejemplos 
mas_comun :: (Eq b) => [Ejemplo a b] -> b
mas_comun ejemplos = mas_comun_aux (map snd (map snd ejemplos)) ((posiblesvalores.fst.snd.head) ejemplos)

mas_comun_aux :: (Eq b) => [b] -> [b] -> b
mas_comun_aux ejemplos posiblesval = maximo [ (x,ocurrencia x ejemplos) | x <- posiblesval ] (head ejemplos,0)

maximo :: [(b,Int)] -> (b,Int) -> b
maximo [] y = fst y
maximo (x:xs) y = if (snd x) > (snd y) then maximo xs x else maximo xs y

ocurrencia :: (Eq a) => a -> [a] -> Int
ocurrencia a [] = 0
ocurrencia a (x:xs) = if a == x then 1 + ocurrencia a xs else ocurrencia a xs

mejor_clasifica :: [Atributo a] -> [Ejemplo a b] -> Atributo a
mejor_clasifica atributos ejemplos = undefined


entropia :: (Eq b) => [Ejemplo a b] -> Double
entropia ejemplos = entropiaaux ejemplos ((posiblesvalores.atributoObjetivo.head) ejemplos) 0

entropiaaux :: (Eq b) => [Ejemplo a b] -> [b] -> Double -> Double
entropiaaux ejemplos [] ac = ac
entropiaaux ejemplos (c:posiblesval) ac = 
            let p = (fromIntegral (ocurrencia c (map (snd.snd) ejemplos))) / (fromIntegral (length ejemplos)) in
            entropiaaux ejemplos posiblesval (ac - p * (logBase (fromIntegral 2)  p))
            


dividir :: (Eq a) => [Ejemplo a b] -> Atributo a -> [[Ejemplo a b]]
dividir ejemplos atributo = dividirac ejemplos pos (posiblesvalores atributo)
        where pos = posicion atributo (head ejemplos)
       

dividirac :: (Eq a) => [Ejemplo a b] -> Int -> [a] -> [[Ejemplo a b]]
dividirac ejemplos pos [] = []
dividirac ejemplos pos (c:posiblesvalores) = (dividiraux ejemplos pos c):(dividirac ejemplos pos posiblesvalores)

dividiraux :: (Eq a) => [Ejemplo a b] -> Int -> a -> [Ejemplo a b]
dividiraux ejemplos pos valor =  [ x | x <- ejemplos, snd ((fst x)!!pos) == valor ]

--filter ((==valor).(map snd).map (!!pos).(map fst)) ejemplos


"""
posicion :: (Eq a) => Atributo a -> Ejemplo a b -> Int
posicion atributo ejemplo = head [y | (y,z) <- zip [0..] ej, z==atributo]
         where ej = map fst (fst ejemplo)


gananciadeinformacion :: [Ejemplo a b] -> Atributo a -> Double
gananciadeinformacion ejemplos atributo =
                      (map (/ns) (map (fromIntegral.length) ej_v))
                      where len = (fromIntegral.length)
                      ns = len ejemplos
                      ej_v = dividir ejemplos atributo

gananciaaux :: [Ejemplo a b] -> Atributo a
"""
