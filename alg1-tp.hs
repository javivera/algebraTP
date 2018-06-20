-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Irina Gallo
-- INTEGRANTE 2: Juan Diego 
-- INTEGRANTE 3: Javier Vera
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t
    
    
-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]
camino4 = [Derecha,Abajo,Derecha,Abajo,Derecha] -- Hecho por Javi

-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]

-------------------- Camino valido -------------------- 
--Recibe posición actual y devuelve la proxima posición en base a la direccion del desplazamiento
nuevaPos :: Desplazamiento -> Posicion -> Posicion
nuevaPos Arriba    (i,j) = (i-1,j)
nuevaPos Abajo     (i,j) = (i+1,j)
nuevaPos Derecha   (i,j) = (i,j+1)
nuevaPos Izquierda (i,j) = (i,j-1)

sigPosSegunCamino :: Posicion -> Camino -> Posicion
sigPosSegunCamino pos (x:camino) = nuevaPos x pos

caminoValidoAux :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoAux tablero camino pos | camino == [] = posValida tablero pos
                                      | posValida tablero pos == False = False
                                      | otherwise = caminoValidoAux tablero (tail camino) (sigPosSegunCamino pos  camino)

caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoAux tablero camino (1,1)
-------------------- Fin Camino valido --------------------



-------------------- Camino de Salida --------------------
caminoDeSalidaAux :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaAux campoMinado camino pos | not (posValida campoMinado pos) = False 
                                            | camino == [] && pos /= (tamano campoMinado,tamano campoMinado) = False 
                                            | camino == [] = not (valor campoMinado pos) 
                                            | valor campoMinado pos == True = False
                                            | otherwise = caminoDeSalidaAux campoMinado (tail camino) (sigPosSegunCamino pos camino)  

caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida campoMinado camino = caminoDeSalidaAux campoMinado camino (1,1)
-------------------- Camino de Salida --------------------

-------------------- Camino de Salida sin Repetidos--------------------
posEnLista :: Posicion -> [Posicion] -> Bool
posEnLista pos listaI | listaI == [] = False
                            | pos == head(listaI) = True
                            | otherwise = posEnLista pos (tail listaI)

caminoDeSalidaSinRepetidosAux :: CampoMinado -> Camino -> Posicion -> [Posicion] -> Bool
caminoDeSalidaSinRepetidosAux campoMinado camino pos listaI | not (posValida campoMinado pos) = False
                                                               | posEnLista pos listaI == True = False
                                                               | camino == [] = not (valor campoMinado pos)
                                                               | valor campoMinado pos == True = False
                                                               | otherwise = caminoDeSalidaSinRepetidosAux campoMinado (tail camino) (sigPosSegunCamino pos camino) ( pos : listaI)             

caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos campoMinado camino = caminoDeSalidaSinRepetidosAux campoMinado camino (1,1) []
-------------------- Camino de Salida sin Repetidos--------------------

-------------------- Salida en K Desplazamientos --------------------
salidaEnKDespAux ::  Conjunto Camino -> CampoMinado -> Conjunto Camino
salidaEnKDespAux listaCaminosPosibles tablero | listaCaminosPosibles == [] = []
                                              | caminoValido tablero (head listaCaminosPosibles) == False = salidaEnKDespAux (tail listaCaminosPosibles) tablero
                                              | caminoDeSalida tablero (head listaCaminosPosibles) == False = salidaEnKDespAux (tail listaCaminosPosibles) tablero 
                                              | otherwise = head listaCaminosPosibles : salidaEnKDespAux (tail listaCaminosPosibles) tablero
 
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino
salidasEnKDesp campo pasos =  salidaEnKDespAux (variaciones pasos [Arriba,Abajo,Izquierda,Derecha]) campo

variaciones :: Integer -> Camino -> [Camino]
variaciones n cs
 | n <= 0 = []
 | n == 1 = map pure cs --  map (\c -> [c]) cs
 | otherwise = variaciones (n-1) cs >>= \ps -> map (:ps) cs
-------------------- Salida en K Desplazamientos --------------------

sigPosSegunCaminoAF :: Posicion -> Desplazamiento -> Posicion
sigPosSegunCaminoAF pos camino |camino == Derecha = (fst pos , (snd pos)+1) 
                                     |camino == Izquierda = (fst pos , (snd pos)-1) 
                                     |camino == Abajo = ( (fst pos)+1 , snd pos) 
                                     |camino == Arriba =( (fst pos)-1 , snd pos)
recorridoAux :: TableroAF -> Posicion -> Integer -> [Posicion]
recorridoAux tablero pos limitador | limitador == 10 = [valor tablero pos]
                                      | not (posValida tablero (sigPosSegunCaminoAF tablero (valor tablero pos))) = [valor tablero pos]
                                      | otherwise = (valor tablero pos) : recorridoAux tablero (sigPosSegunCaminoAF pos (valor tablero pos)) (limitador-1)

