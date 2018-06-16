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

------------------- Funciones Auxiliares agregadas por nosotros -------------------

--Recibe indice actual y devuelve el proximo indice en base a la direccion del desplazamiento
sigIndiceSegunCamino :: Posicion -> Desplazamiento -> Posicion
sigIndiceSegunCamino indice direccion | direccion == Derecha = (fst indice , (snd indice)+1) 
                                      | direccion == Izquierda = (fst indice , (snd indice)-1) 
                                      | direccion == Abajo = ( (fst indice)+1 , snd indice) 
                                      | direccion == Arriba =( (fst indice)-1 , snd indice)

-------------------- Camino valido -------------------- 
caminoValidoAux :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoAux tablero camino indice | camino == [] = posValida tablero indice
                                      | posValida tablero indice == False = False
                                      | otherwise = caminoValidoAux tablero (tail camino) (sigIndiceSegunCamino indice (head camino))

caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoAux tablero camino (1,1)
-------------------- Fin Camino valido --------------------



-------------------- Camino de Salida --------------------
caminoDeSalidaAux :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaAux campoMinado camino indice | not (posValida campoMinado indice) = error " Camino no válido..." 
                                            | camino == [] = not (valor campoMinado indice)
                                            | valor campoMinado indice == True = False
                                            | otherwise = caminoDeSalidaAux campoMinado (tail camino) (sigIndiceSegunCamino indice (head camino))  

caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida campoMinado camino = caminoDeSalidaAux campoMinado camino (1,1)
-------------------- Camino de Salida --------------------

