module Funciones where


type Ciudad = String
type Duracion = Float
type Duraciones = [Duracion]
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]
type Tramos = [(Ciudad, Ciudad)]
type Ciudades = [Ciudad]

obtDur :: Vuelo -> Duracion
obtDur (_, _, duracion) = duracion

obtCiudPrim :: Vuelo -> Ciudad
obtCiudPrim (ciudad, _, _) = ciudad

obtCiudSeg :: Vuelo -> Ciudad
obtCiudSeg (_, ciudad, _) = ciudad

durIguales :: Vuelo -> Vuelo -> Bool
durIguales vuelo1 vuelo2 = (obtDur vuelo1 == obtDur vuelo2)

ciudPrimIguales :: Vuelo -> Vuelo -> Bool
ciudPrimIguales vuelo1 vuelo2 = (obtCiudPrim vuelo1 == obtCiudPrim vuelo2)

ciudSegIguales :: Vuelo -> Vuelo -> Bool
ciudSegIguales vuelo1 vuelo2 = (obtCiudSeg vuelo1 == obtCiudSeg vuelo2)

vuelosIguales :: Vuelo -> Vuelo -> Bool
vuelosIguales vuelo1 vuelo2 = (ciudPrimIguales vuelo1 vuelo2) &&
                                (ciudSegIguales vuelo1 vuelo2) &&
                                (durIguales vuelo1 vuelo2)

ciudadesIguales :: Vuelo -> Vuelo -> Bool
ciudadesIguales vuelo1 vuelo2 = (ciudPrimIguales vuelo1 vuelo2) &&
                                (ciudSegIguales vuelo1 vuelo2)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:[]) = False
hayRepetidos (x:xs) = (elem x xs) || (hayRepetidos xs)


obtenerTramos :: AgenciaDeViajes -> Tramos
obtenerTramos [] = []
obtenerTramos (vuelo:vuelos) = (obtCiudPrim vuelo, obtCiudSeg vuelo):(obtenerTramos vuelos)


todosVuelosValidos :: AgenciaDeViajes -> Bool
todosVuelosValidos [] = True
todosVuelosValidos (vuelo:vuelos) = (vueloValido vuelo) && (todosVuelosValidos vuelos)  


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    | elem x xs = eliminarRepetidos xs
    | otherwise = x:(eliminarRepetidos xs)

-- ejercicio 1 ------

vueloValido :: Vuelo -> Bool
vueloValido vuelo = (obtDur vuelo > 0) && (obtCiudPrim vuelo /= obtCiudSeg vuelo)


vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos vuelos = (todosVuelosValidos vuelos) && (not (hayRepetidos vuelos)) &&  not (hayRepetidos (obtenerTramos vuelos))






-- vuelosPrueba :: AgenciaDeViajes
-- vuelosPrueba = [
--     ("Madrid", "Barcelona", 1.5),  -- Válido
--     ("Buenos Aires", "Córdoba", 2.0),  -- Válido
--     ("Madrid", "Barcelona", 1.5),  -- Repetido (invalid)
--     ("Barcelona", "Barcelona", 1.0),  -- Ciudad1 = Ciudad2 (invalid)
--     ("Lima", "Santiago", -2.0),  -- Tiempo <= 0 (invalid)
--     ("Madrid", "Valencia", 2.0),  -- Válido
--     ("Córdoba", "Buenos Aires", 3.0),  -- Válido
--     ("Madrid", "Valencia", 1.5),  -- Diferente duración para el mismo par de ciudades (invalid)
--     ("Córdoba", "Santiago", 1.0)  -- Válido
-- ]

--[("Madrid", "Barcelona", 1.5), ("Buenos Aires", "Córdoba", 2.0), ("Madrid", "Barcelona", 2)]



-- ejercicio 2 ------
ciudadesConectadas :: AgenciaDeViajes -> Ciudad ->  Ciudades
ciudadesConectadas agencia ciudad = eliminarRepetidos (ciudadesConectadasAux agencia ciudad)

ciudadesConectadasAux :: AgenciaDeViajes -> Ciudad ->  Ciudades
ciudadesConectadasAux [] _ = []
ciudadesConectadasAux (vuelo:vuelos) ciudad
    | ciudad == obtCiudPrim vuelo = obtCiudSeg vuelo: ciudadesConectadasAux vuelos ciudad
    | ciudad == obtCiudSeg vuelo = obtCiudPrim vuelo: ciudadesConectadasAux vuelos ciudad
    | otherwise = ciudadesConectadasAux vuelos ciudad


-- ejercicio 3 --

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota (vuelo:vuelos) = (obtCiudPrim vuelo, obtCiudSeg vuelo, (obtDur vuelo) * 0.9):(modernizarFlota vuelos)

-- ejercicio 4 --

contarAparicionCiudad :: Ciudades -> Ciudad -> Int
contarAparicionCiudad [] _ = 0
contarAparicionCiudad (ciudad:ciudades) ciudadABuscar 
    | ciudad == ciudadABuscar = 1 + contarAparicionCiudad ciudades ciudadABuscar
    | otherwise = contarAparicionCiudad ciudades ciudadABuscar

unirTramos :: AgenciaDeViajes -> Ciudades
unirTramos [] = []
unirTramos (vuelo:vuelos) = (obtCiudPrim vuelo):(obtCiudSeg vuelo):(unirTramos vuelos)


ciudadMasConectadaAux :: Ciudades -> Ciudades -> Ciudad
ciudadMasConectadaAux _ (ciudad:[]) = ciudad
ciudadMasConectadaAux lista_ciudades (ciudad1:ciudad2:ciudades)
    | contarAparicionCiudad lista_ciudades ciudad1 > contarAparicionCiudad lista_ciudades ciudad2 = ciudadMasConectadaAux lista_ciudades (ciudad1:ciudades)
    | otherwise = ciudadMasConectadaAux lista_ciudades (ciudad2:ciudades)

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada ciudades = ciudadMasConectadaAux (unirTramos ciudades) (eliminarRepetidos (unirTramos  ciudades))

-- ejercicio 5 --

-- vueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
-- vueloDirecto (vuelo:vuelos) origen destino  = elem destino (conseguirDestinos (vuelo:vuelos) origen)


conseguirDestinos :: AgenciaDeViajes -> Ciudad -> Ciudades
conseguirDestinos [] _ = []
conseguirDestinos (ciudad:ciudades) origen 
    | origen == obtCiudPrim ciudad = (obtCiudSeg ciudad):(conseguirDestinos(ciudades) origen)
    | otherwise = conseguirDestinos(ciudades) origen

conseguirDestinosMultiples :: AgenciaDeViajes -> Ciudades -> Ciudades
conseguirDestinosMultiples _ [] = []
conseguirDestinosMultiples agencia (origen:origenes) = conseguirDestinos agencia origen ++ conseguirDestinosMultiples agencia origenes


sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar agencia origen distino = elem distino (distinosDirectos ++ (conseguirDestinosMultiples agencia distinosDirectos))
    where distinosDirectos = conseguirDestinos agencia origen


-- ejercicio 6 --

-- t = la agencia de viajes, pero sin ir avanzando sobre la misma con la recursividad, queda estatica
-- Primera guarda : Si existe un vuelo directo, agrega la duracion del mismo a la lista (la lista sera el resultado)
-- Segunda guarda : Si la 1ra ciudad pertenece al conjunto de ciudades destino del origen, agrega la duracion de ambos vuelos
-- EJ 2da Guard. -> Buenos Aires --5-> Cordoba && Cordoba --4-> Santa Fe
--               -> ciudades destino de Buenos aires = ["Cordoba"]
--               -> Quiero hallar la duracion de Bs -> Santa Fe
--               -> Cuando analize el vuelo Cordoba -> Santa Fe, cordoba pertenece a ciudades destino de bs? Si, entonces la 
--               ->  - duracion que agrega es 4 + 5 (toma ambos vuelos)
--               -> Una vez obtenidas todas las duraciones para ir de A->B, damos como resultado al elemento minimo de la lista.

buscarDuracion :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
buscarDuracion [] _ _ = -1
buscarDuracion (vuelo:vuelos) origen destino 
    | vuelo == (origen, destino, (obtDur vuelo)) = (obtDur vuelo)
    | otherwise = buscarDuracion vuelos origen destino


conseguirEscalasCompletasAux :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> Duraciones
conseguirEscalasCompletasAux [] _ _ _ = []
conseguirEscalasCompletasAux (vuelo:vuelos) vuelosFijo origen destino 
    | ((c1 == origen) && (c2 == destino)) = (obtDur vuelo):(conseguirEscalasCompletasAux vuelos vuelosFijo origen destino)
    | ((elem (c1) (destinosDelOrigen)) && (c2 == destino)) = ((obtDur vuelo) + buscarDuracion vuelosFijo origen c1):(conseguirEscalasCompletasAux vuelos vuelosFijo origen destino)
    | otherwise = (conseguirEscalasCompletasAux vuelos vuelosFijo origen destino)
    where c1 = obtCiudPrim vuelo
          c2 = obtCiudSeg vuelo
          destinosDelOrigen = conseguirDestinos vuelosFijo origen


conseguirEscalasCompletas :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duraciones
conseguirEscalasCompletas t o d = conseguirEscalasCompletasAux t t o d


minimaDuracionAux :: Duraciones -> Float -> Duracion
minimaDuracionAux [] n = n
minimaDuracionAux (tiempo:tiempos) n
    | n == 0 || tiempo < n = minimaDuracionAux tiempos tiempo
    | otherwise = minimaDuracionAux tiempos n
    

minimaDuracion :: Duraciones -> Duracion
minimaDuracion x = minimaDuracionAux x 0


duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido agencia origen destino = minimaDuracion (conseguirEscalasCompletas agencia origen destino)






-- ejercicio 7--

-- pertenece :: (Eq t) => t -> [t] -> Bool
-- pertenece _ [] = False
-- pertenece e (x:xs) = (e == x) || (pertenece e xs)


-- agregarVueloAAgencia :: Vuelo -> AgenciaDeViajes -> AgenciaDeViajes
-- agregarVueloAAgencia vuelo agencia = (vuelo:agencia)


-- eliminarCiudadDeAgencia :: Ciudad -> AgenciaDeViajes -> AgenciaDeViajes -- elimina todod los viajes que tienen esta ciudad
-- eliminarCiudadDeAgencia _ [] = []
-- eliminarCiudadDeAgencia ciudad (viaje:agencia)
--     | (ciudad == obtCiudPrim viaje) || (ciudad == obtCiudSeg viaje) = eliminarCiudadDeAgencia ciudad agencia
--     | otherwise = viaje:(eliminarCiudadDeAgencia ciudad agencia)

-- ciudadesAnteriores :: Visitado -> Visitado -- visitado tiene por lo menos 2 ciudades ya que visitamos el origin y la ciudad a donde viajamos
-- ciudadesAnteriores (_:ciudades) = ciudades

-- ciudadDeEstadia :: Visitado -> Ciudad
-- ciudadDeEstadia (ciudad_de_estadia:ciudades) = ciudad_de_estadia


-- viajar :: Turista -> Turista -- viaja a la primera ciudad que aparece en la lista conseguir destinos
-- viajar (agencia, visitado)
-- -- callejos sin salida(estamos seguros que objetivo no es callejon sin salida)
--     | (conseguirDestinos agencia (ciudadDeEstadia visitado) == []) = (eliminarCiudadDeAgencia (ciudadDeEstadia visitado) agencia, ciudadesAnteriores visitado) -- dar un paso atras y eliminar callejos sin salida
--     | otherwise = (agencia, (ciudad_de_estadia:visitado))
--     where ciudad_de_estadia = ciudadDeEstadia (conseguirDestinos agencia (ciudadDeEstadia visitado)) -- basicamente eligimos la primera ciudad en la lista de destinos



-- -- bucle es cuando Visitado es semejante a lo siguiente : [A, b1, b2,..., bn, A, T,..., origen]
-- -- A siempre esta en el inicio de la lista(es decir en la ciudad de estadia)
-- -- entonces tenemos que convertirlo en: [origen] conectando origen con todos los destinos
-- -- de todos los bi que habia -> (origen, distino bi, tiempo)
-- -- A es inicio de bucle

-- hayBucle :: Visitado -> Bool
-- hayBucle visitado = pertenece (ciudadDeEstadia visitado) (ciudadesAnteriores visitado)

-- hayBucleConObjetivo :: Ciudad -> Visitado -> Bool
-- hayBucleConObjetivo objetivo visitado = (hayBucle visitado) && (ciudadDeEstadia visitado == objetivo)

-- obtBucle :: Bool -> Ciudad -> Visitado -> Ciudades -- requiere: existe bucle
-- obtBucle es_inicio inicio_de_bucle (ciudad:ciudades)
--     | (es_inicio == True) = ciudad:(obtBucle False inicio_de_bucle (ciudades))
--     | (ciudad == inicio_de_bucle) = [inicio_de_bucle]
--     | otherwise = ciudad:(obtBucle False inicio_de_bucle ciudades)


-- obtTodosDestinosDeBucle :: Ciudades -> AgenciaDeViajes -> Ciudades -- todas las ciudades a las cuales se puede llegar en un paso desde cualquer ciudad de bucle
-- obtTodosDestinosDeBucle bucle agencia = eliminarRepetidos (conseguirDestinosMultiples agencia bucle)


-- eliminarBucleDeAgencia :: Ciudades -> AgenciaDeViajes -> AgenciaDeViajes
-- eliminarBucleDeAgencia [] agencia = agencia
-- eliminarBucleDeAgencia (elemento_de_bucle:resto_de_bucle) agencia = eliminarBucleDeAgencia resto_de_bucle (eliminarCiudadDeAgencia elemento_de_bucle agencia)


-- conectarCiudadConDestinos :: Ciudad -> Ciudades -> AgenciaDeViajes -> AgenciaDeViajes
-- conectarCiudadConDestinos origen [] agencia = agencia
-- conectarCiudadConDestinos origen (destino:destinos) agencia
--     | (elem (origen, destino) (obtenerTramos agencia)) || (origen == destino) == True = conectarCiudadConDestinos origen destinos agencia
--     | otherwise = conectarCiudadConDestinos origen destinos (nueva_agencia)
--     where nueva_agencia = agregarVueloAAgencia (origen, destino, 1) agencia


-- eliminarBucleAux :: Ciudad -> Turista -> Ciudades -> Turista
-- eliminarBucleAux origen (agencia, visitado) bucle = (nueva_agencia, [origen]) -- [objetivo] volvemos al origen
--     where nueva_agencia = (conectarCiudadConDestinos (origen) (obtTodosDestinosDeBucle bucle agencia) (eliminarBucleDeAgencia bucle agencia))


-- eliminarBucle :: Ciudad -> Turista -> Turista
-- eliminarBucle origen (agencia, visitado)
--     | (hayBucle visitado) == True = eliminarBucleAux origen (agencia, visitado) (obtBucle True (ciudadDeEstadia visitado) visitado)
--     | otherwise = (agencia, visitado)


-- buscarOrigen :: Turista -> Ciudad -> Bool
-- buscarOrigen (agencia, visitado) objetivo
--     | elem objetivo (conseguirDestinos agencia (ciudadDeEstadia visitado)) == True = True
--     | (conseguirDestinos agencia objetivo) == [] = False
--     | otherwise = buscarOrigen (eliminarBucle objetivo (viajar (agencia, visitado))) objetivo

-- puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
-- puedoVolverAOrigen agencia origen = buscarOrigen (agencia, [origen]) origen

