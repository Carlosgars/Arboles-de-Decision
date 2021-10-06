module Tipos where

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

showTree :: (Show a, Show b) => (Arbol a b) -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.("|\n"++).showList [hijo a | a <- posiblesvalores atrib].('>':)
