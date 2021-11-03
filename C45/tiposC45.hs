module TiposC45
where

import Data.Either

-- Tipo Discreto --

data Discreto =
     D { dnombre :: String, posiblesvalores :: [String] }
        deriving Eq

instance Show Discreto where
  show = dnombre

-- Tipo Continuo --

data Continuo =
     C { cnombre :: String, rango :: (Double,Double), umbral :: Maybe Double }

instance Eq Continuo where
     continuo == continuo' =
              cnombre continuo == cnombre continuo'
              && rango continuo == rango continuo'
     
instance Show Continuo where
  show = cnombre

-- Tipo Atributo --

type Atributo = Either Discreto Continuo
type ValorAtrib = Either String Double

nombre :: Atributo -> String
nombre (Left atributo) = dnombre atributo
nombre (Right atributo) = cnombre atributo

posiblesval :: Atributo -> [String]
posiblesval (Left atributo) = posiblesvalores atributo
posiblesval (Right atributo) = ["<=",">"]

--Tipo Ejemplo --

type Ejemplo = ( [(Atributo, ValorAtrib)], (Atributo, ValorAtrib) )

-- Tipo Arbol --

data Arbol = Hoja String
     | Nodo { atrib::Atributo, hijo::String -> (Arbol) }

instance Show Arbol where
     show x = showTree x ""

showTree :: Arbol -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.("|\n"++).showList [hijo a | a <- posiblesval atrib].('>':)



