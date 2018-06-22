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

sigIndiceSegunCaminoAF :: Posicion -> TableroAF -> Posicion
sigIndiceSegunCaminoAF indice tablero |valor tablero indice == Derecha = (fst indice , (snd indice)+1) 
                                      |valor tablero indice == Izquierda = (fst indice , (snd indice)-1) 
                                      |valor tablero indice == Abajo = ( (fst indice)+1 , snd indice) 
                                      |valor tablero indice == Arriba =( (fst indice)-1 , snd indice)

                                     
                                     
recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido tablero indice | not (posValida tablero (sigIndiceSegunCaminoAF indice tablero)) = [indice]
                         | otherwise = indice : recorrido tablero (sigIndiceSegunCaminoAF indice tablero)


escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero tablero posicionInicial | not (posValida tablero (sigIndiceSegunCaminoAF posicionInicial tablero)) = True
                                         | otherwise = escapaDelTablero tablero (sigIndiceSegunCaminoAF posicionInicial tablero) 

--------------------------No me acuerdo como usar el commit, asique hice push nomas, de arriba no cambie nada.
--------------------------Igual, deberiamos cambiar "indice" por "pos" y creo que "campoMinado" por "cm"
--nuevoTableroaux :: TableroAF -> Posicion -> TableroAF
--nuevoTableroaux tablero indice | indice == (1,1)

--recibis la posicion, (1,1)
--		     (1,_)
--                     (_,1)
--		     otherwise

--agregue valor en posicion k de una lista,
--insertar [[asd]] 

-- Modifica el desplazamiento en (i,j), en el sentido de las agujas del reloj. (Arriba,Derecha,Abajo,Izquierda,Arriba..)
valor2 :: TableroAF -> Posicion -> Desplazamiento
valor2 t (i,j) | desp == Arriba    = Derecha
	       | desp == Derecha   = Abajo
               | desp == Abajo     = Izquierda
               | desp == Izquierda = Arriba
	       where desp = valor t (i,j)

-- Ver --> https://code.i-harness.com/es/q/594e32

----------- (Iri) De arriba solo arreglé los nombres. 
----agrego unas funciones auxiliares para el ultimo ejercicio
-- Gira desplazamiento en lista de listas:

-- Recibe un desplazamiento, y lo gira sentido horario (arriba, derecha, abajo, izquierda...)
girar :: Desplazamiento -> Desplazamiento
girar Arriba = Derecha
girar Derecha = Abajo
girar Abajo = Izquierda
girar Izquierda = Arriba

--Recibe un Camino y un valor "K". Gira el desplazamiento (en sentido horario) que está en K: 
giraEnCamino :: Camino -> Integer -> Camino
giraEnCamino camino 1 = ( girar (head camino) : tail camino )
giraEnCamino camino k = ( head camino : giraEnCamino (tail camino) (k-1) )

--Recibe un tableroAF y una posición (i,j). Gira el desplazamiento de la posición j (columna) que está en la lista i (fila)

giraEnTableroAF :: TableroAF -> Posicion -> TableroAF
giraEnTableroAF tablero (1, j) = ( giraEnCamino (head tablero) j : tail tablero )
giraEnTableroAF tablero (i, j) = ( head tablero : giraEnTableroAF (tail tablero) (i-1, j) )
