module EjemplosSexo where

import Tipos
import AuxID3
import ID3
import Utils

ejemplo1 = ([(altura,"alto"),(peso,"ligero")],(sexo,"hombre")) :: Ejemplo String String
ejemplo2 = ([(altura,"medio"),(peso,"pesado")],(sexo,"hombre")) :: Ejemplo String String
ejemplo3 = ([(altura,"bajo"),(peso,"ligero")],(sexo,"mujer")) :: Ejemplo String String
ejemplo4 = ([(altura,"medio"),(peso,"ligero")],(sexo,"mujer")) :: Ejemplo String String
ejemplo5 = ([(altura,"bajo"),(peso,"pesado")],(sexo,"mujer")) :: Ejemplo String String

desconocido = ["alto","pesado"]

ejemplosSexo = [ejemplo1,ejemplo2,ejemplo3,ejemplo4,ejemplo5] :: [Ejemplo String String]

sexo = A "sexo" ["hombre","mujer"] :: Atributo String
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

atributosSexo = [altura,peso] :: [Atributo String]
