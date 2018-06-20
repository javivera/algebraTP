type Set a = [a]

agregarATodas :: Integer -> Set [Integer] -> Set [Integer]
agregarATodas x cls | cls == [] = []
                    | otherwise = (x:(head cls)) : (agregarATodas x (tail cls))

transformaElementosEnConjuntos :: Set Integer -> Set [Integer]
transformaElementosEnConjuntos c | length c == 1 = [[head c]]
                                 | otherwise = [head c] : (transformaElementosEnConjuntos (tail c))

agregarTodosATodas :: Set Integer -> Set [Integer] -> Set [Integer]
agregarTodosATodas c1 c2 | c1 == [] = []
                         | otherwise = (agregarATodas (head c1) c2) ++ (agregarTodosATodas (tail c1) c2)

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones c x | x == 1 = transformaElementosEnConjuntos c
                | otherwise =  agregarTodosATodas c (variaciones c (x-1))


