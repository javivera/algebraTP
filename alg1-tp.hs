-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Irina Gallina
-- INTEGRANTE 2: Juan Diego Figari
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
--Recibe posición (i,j) y devuelve la nueva posicion en base a la direccion del desplazamiento.
nuevaPos :: Posicion -> Camino -> Posicion
nuevaPos (i,j) (Derecha   : camino) = (i,j+1)
nuevaPos (i,j) (Izquierda : camino) = (i,j-1) 
nuevaPos (i,j) (Abajo     : camino) = (i+1,j) 
nuevaPos (i,j) (Arriba    : camino) = (i-1,j) 


--Recibe un tablero (t), un camino y una posicion (pos).
caminoValidoAux :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoAux t camino pos | camino == [] = posValida t pos
                                   | not (posValida t pos) = False
                                   | otherwise = caminoValidoAux t (tail camino) (nuevaPos pos  camino)

--Recibe un tablero (t) y un camino. Determina si siguiendo el camino dado se sale del tablero o no.
caminoValido :: Tablero a -> Camino -> Bool
caminoValido t camino = caminoValidoAux t camino (1,1)
-------------------- Fin Camino valido --------------------



-------------------- Camino de Salida --------------------

--Recibe un campo minado (cm), un camino, y una posicion (pos).
caminoDeSalidaAux :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaAux cm camino pos | not (posValida cm pos) = False 
                                | camino == [] && pos /= (tamano cm,tamano cm) = False
                                | camino == [] = not (valor cm pos)
                                | valor cm pos == True = False
                                | otherwise = caminoDeSalidaAux cm (tail camino) (nuevaPos pos camino)

--Determina si pisa o no una mina durante el camino, empezando en el (1,1).
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida cm camino = caminoDeSalidaAux cm camino (1,1)
-------------------- Camino de Salida --------------------



-------------------- Camino de Salida sin Repetidos --------------------
--Determina si una posicion (pos), pertenece a la lista de posiciones anteriores a esa (listaI).
posEnLista :: Posicion -> [Posicion] -> Bool
posEnLista pos listaI | listaI == [] = False
                      | pos == head(listaI) = True
                      | otherwise = posEnLista pos (tail listaI)


caminoDeSalidaSinRepAux :: CampoMinado -> Camino -> Posicion -> [Posicion] -> Bool
caminoDeSalidaSinRepAux cm camino pos listaI | not (posValida cm pos) = False 
                                             | posEnLista pos listaI == True = False 
                                             | camino == [] = not (valor cm pos)
                                             | valor cm pos == True = False 
                                             | otherwise = caminoDeSalidaSinRepAux cm (tail camino) (nuevaPos pos camino) ( pos : listaI)

caminoDeSalidaSinRep :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRep cm camino = caminoDeSalidaSinRepAux cm camino (1,1) []
-------------------- Camino de Salida sin Repetidos --------------------



-------------------- Salida en K Desplazamientos --------------------

--Auxiliares de variaciones: reciben desplazamientos (desp), un camino y conjuntos de caminos (cjtoCamino). 
--Variaciones devuelve un conjunto de caminos de longitud "n", con las combinaciones posibles de los desplazamientos.
agregar:: Desplazamiento -> Camino -> Camino
agregar desp camino = (desp : camino)

agregarATodas:: Desplazamiento -> Conjunto Camino -> Conjunto Camino
agregarATodas desp []= []
agregarATodas desp cjtoCamino = (agregar desp (head cjtoCamino)) : (agregarATodas desp (tail cjtoCamino)) 

auxVariaciones:: Camino -> Conjunto Camino -> Conjunto Camino
auxVariaciones [] cjtoCamino = []
auxVariaciones camino cjtoCamino = (agregarATodas (head camino) cjtoCamino) ++ auxVariaciones (tail camino) cjtoCamino

variaciones:: Camino -> Integer -> Conjunto Camino
variaciones camino 0 = [[]]
variaciones camino n = auxVariaciones camino (variaciones camino (n-1)) 


--Recibe una lista de caminos posibles (listaCaminos) y un campo minado (cm), devuelve los caminos que llegan bien al final.
salidasEnKDespAux ::  Conjunto Camino -> CampoMinado -> Conjunto Camino
salidasEnKDespAux listaCaminos cm | listaCaminos == [] = [] 
                                  | caminoValido cm (head listaCaminos) == False = salidasEnKDespAux (tail listaCaminos) cm 
                                  | caminoDeSalida cm (head listaCaminos) == False = salidasEnKDespAux (tail listaCaminos) cm 
                                  | otherwise = (head listaCaminos) : salidasEnKDespAux (tail listaCaminos) cm 

--Recibe un campo minado (cm) y una cantidad de pasos (n). Se fija todos los caminos posibles que funcionan con n.
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino
salidasEnKDesp cm n =  salidasEnKDespAux (variaciones [Arriba,Abajo,Izquierda,Derecha] n) cm
-------------------- Salida en K Desplazamientos --------------------



-------------------- Recorrido --------------------
--Recibe una posicion (i,j) y un tableroAF (taf). Mueve la posición según el desplazamiento que hay en esa posición.
nuevaPosAF :: Posicion -> TableroAF -> Posicion
nuevaPosAF (i,j) taf |valor taf (i,j) == Derecha   = (i,j+1)
                     |valor taf (i,j) == Izquierda = (i,j-1)
                     |valor taf (i,j) == Abajo     = (i+1,j)
                     |valor taf (i,j) == Arriba    = (i-1,j)

--Recibe un tableroAF (taf) y una posicion (pos), devuelve una lista que contiene todas las posiciones por la que pasó el AF antes de salir del tablero.
recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido tablero pos | not (posValida tablero (nuevaPosAF pos tablero)) = [pos]
                      | otherwise = pos : recorrido tablero (nuevaPosAF pos tablero)
-------------------- Recorrido --------------------



-------------------- Escapa del tablero --------------------
perteneceLista :: Posicion -> [Posicion] -> Bool
perteneceLista posicion listaPos | listaPos == [] = False
                                 | posicion == head(listaPos) = True
                                 | otherwise = perteneceLista posicion (tail listaPos)

escapaDelTableroAux :: TableroAF -> Posicion -> Posicion -> Int -> Bool
escapaDelTableroAux taf posMovil posInicial n  | not (posValida taf (nuevaPosAF posInicial taf)) = True
                                               | perteneceLista posMovil (take n (recorrido taf posInicial)) = False 
                                               | otherwise = escapaDelTableroAux taf (nuevaPosAF posMovil taf) posInicial (n+1)

escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero taf posicion = escapaDelTableroAux taf posicion posicion 0                                                     
-------------------- Escapa del tablero --------------------



-------------------- Cantidad de pasos para salir --------------------

--Recibe un desplazamiento, y lo gira sentido horario (Arriba, Derecha, Abajo, Izquierda, Arriba...).
girar :: Desplazamiento -> Desplazamiento
girar Arriba = Derecha
girar Derecha = Abajo
girar Abajo = Izquierda
girar Izquierda = Arriba

--Recibe un camino y un valor (K). Gira el desplazamiento (en sentido horario) que está en K: 
giraEnCamino :: Camino -> Integer -> Camino
giraEnCamino camino 1 = (girar (head camino) : tail camino)
giraEnCamino camino k = (head camino : giraEnCamino (tail camino) (k-1))

--Recibe un tableroAF (taf) y una posición (i,j). Gira el desplazamiento de la posición j (columna) que está en la lista i (fila).
giraEnTableroAF :: TableroAF -> Posicion -> TableroAF
giraEnTableroAF taf (1,j) = (giraEnCamino (head taf) j : tail taf)
giraEnTableroAF taf (i,j) = (head taf : giraEnTableroAF (tail taf) (i-1,j))

--Recibe un tableroAF (taf) y una posicion (pos). Devuelve una lista con todas las posiciones hasta salir.
cantidadDePasosParaSalirAux :: TableroAF -> Posicion -> [Posicion]
cantidadDePasosParaSalirAux taf pos | not (posValida taf pos) = [] 
                                    | otherwise = pos : cantidadDePasosParaSalirAux (giraEnTableroAF taf pos) (nuevaPosAF pos taf)

--Cuenta la cantidad de posiciones que hay en la lista de su funcion auxiliar.
cantidadDePasosParaSalir :: TableroAF -> Posicion -> Int
cantidadDePasosParaSalir t pos = length (cantidadDePasosParaSalirAux t pos)
-------------------- Cantidad de pasos para salir --------------------


