module TiposCART
where

import Data.Either
import Data.Maybe

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
    
--Tipo Ejemplo --

type Ejemplo = ( [(Atributo, ValorAtrib)], (Atributo, ValorAtrib) )

-- Tipo Arbol --

data Arbol = Hoja ValorAtrib
     | Nodo { atrib::Atributo, hijo::String -> (Arbol) }

instance Show Arbol where
     show x = showTree x ""

showTree :: Arbol -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.(showUmbral atrib ++).("|\n"++).showList [(hijo a,a) | a <- posiblesval atrib].('>':)


-- Funciones

nombre :: Atributo -> String
nombre (Left atributo) = dnombre atributo
nombre (Right atributo) = cnombre atributo

posiblesval :: Atributo -> [String]
posiblesval (Left atributo) = posiblesvalores atributo
posiblesval (Right atributo) = ["<=",">"]

showUmbral :: Atributo -> String
showUmbral (Left atrib) = ""
showUmbral (Right atrib) =
    let u = umbral atrib
    in if isNothing u then ""
    else " {Umbral: " ++ (show $ fromJust $ u) ++ "}"


