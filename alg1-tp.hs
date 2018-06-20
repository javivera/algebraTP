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
--Recibe indice actual y devuelve el proximo indice en base a la direccion del desplazamiento
sigIndiceSegunCamino :: Posicion -> Camino -> Posicion
sigIndiceSegunCamino indice camino | head camino == Derecha = (fst indice , (snd indice)+1) 
                                   | head camino == Izquierda = (fst indice , (snd indice)-1) 
                                   | head camino == Abajo = ( (fst indice)+1 , snd indice) 
                                   | head camino == Arriba =( (fst indice)-1 , snd indice)


caminoValidoAux :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoAux tablero camino indice | camino == [] = posValida tablero indice
                                      | posValida tablero indice == False = False
                                      | otherwise = caminoValidoAux tablero (tail camino) (sigIndiceSegunCamino indice  camino)

caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoAux tablero camino (1,1)
-------------------- Fin Camino valido --------------------



-------------------- Camino de Salida --------------------
caminoDeSalidaAux :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaAux campoMinado camino indice | not (posValida campoMinado indice) = False 
                                            | camino == [] && indice /= (tamano campoMinado,tamano campoMinado) = False 
                                            | camino == [] = not (valor campoMinado indice) 
                                            | valor campoMinado indice == True = False
                                            | otherwise = caminoDeSalidaAux campoMinado (tail camino) (sigIndiceSegunCamino indice camino)  

caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida campoMinado camino = caminoDeSalidaAux campoMinado camino (1,1)
-------------------- Camino de Salida --------------------

-------------------- Camino de Salida sin Repetidos--------------------
indiceEnLista :: Posicion -> [Posicion] -> Bool
indiceEnLista indice listaI | listaI == [] = False
                            | indice == head(listaI) = True
                            | otherwise = indiceEnLista indice (tail listaI)

caminoDeSalidaSinRepetidosAux :: CampoMinado -> Camino -> Posicion -> [Posicion] -> Bool
caminoDeSalidaSinRepetidosAux campoMinado camino indice listaI | not (posValida campoMinado indice) = False
                                                               | indiceEnLista indice listaI == True = False
                                                               | camino == [] = not (valor campoMinado indice)
                                                               | valor campoMinado indice == True = False
                                                               | otherwise = caminoDeSalidaSinRepetidosAux campoMinado (tail camino) (sigIndiceSegunCamino indice camino) ( indice : listaI)             

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

sigIndiceSegunCaminoAF :: Posicion -> Desplazamiento -> Posicion
sigIndiceSegunCaminoAF indice camino |camino == Derecha = (fst indice , (snd indice)+1) 
                                     |camino == Izquierda = (fst indice , (snd indice)-1) 
                                     |camino == Abajo = ( (fst indice)+1 , snd indice) 
                                     |camino == Arriba =( (fst indice)-1 , snd indice)

                                     
                                     
recorridoAux :: TableroAF -> Posicion -> Integer -> [Posicion]
recorridoAux tablero indice limitador | limitador == 10 = [valor tablero indice]
                                      | not (posValida tablero (sigIndiceSegunCaminoAF tablero (valor tablero indice))) = [valor tablero indice]
                                      | otherwise = (valor tablero indice) : recorridoAux tablero (sigIndiceSegunCaminoAF indice (valor tablero indice)) (limitador-1)


