module DebugC45
where

import C45
import TiposC45
import AuxC45
import DiscretizarContinuo
import UtilsC45
import GananciaNormalizada
import EjemplosC45

-- Primera iteracion
atribs =  [Left altura, Right peso]
discretizados1 = map (discretizar ejemplos) atribs
mejoratributo = mejorclasifica discretizados1 ejemplos
mejoratributo2 = mejorclasifica2 discretizados1 ejemplos
ejemplosalto = evaluar ejemplos (Left altura)  "alto"
ejemplosmedio = evaluar ejemplos (Left altura)  "medio"
ejemplosbajo = evaluar ejemplos (Left altura)  "bajo"

-- Segunda iteracion
atribs2 =  elimina mejoratributo2 discretizados1

-- rama "alto"
discretizados2alto = map (discretizar ejemplosalto) atribs2
mejoratributo2alto = mejorclasifica2 discretizados2alto ejemplosalto
ejemplosaltomenor = evaluar ejemplosalto (Right peso)  "<="
ejemplosaltomayor = evaluar ejemplosalto (Right peso)  ">"

-- rama "medio"
discretizados2medio = map (discretizar ejemplosmedio) atribs2
mejoratributo2medio = mejorclasifica2 discretizados2medio ejemplosmedio
ejemplosmediomenor = evaluar ejemplosmedio (Right peso)  "<="
ejemplosmediomayor = evaluar ejemplosmedio (Right peso)  ">"

-- rama "bajo"
discretizados2bajo = map (discretizar ejemplosbajo) atribs2
mejoratributo2bajo = mejorclasifica2 discretizados2bajo ejemplosbajo
ejemplosbajomenor = evaluar ejemplosbajo (Right peso)  "<="
ejemplosbajomayor = evaluar ejemplosbajo (Right peso)  ">"