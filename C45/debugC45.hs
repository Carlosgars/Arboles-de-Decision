module DebugC45
where

import C45
import TiposC45
import AuxC45
import DiscretizarContinuo
import UtilsC45
import GananciaNormalizada
import EjemplosC45

atribs =  [Left altura, Right peso]
discretizados = map (discretizar ejemplos) atribs
mejoratributo = mejorclasifica discretizados ejemplos

