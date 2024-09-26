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
conseguirEscalasCompletasAux (vuelo:vuelos) t origen destino 
    | ((c1 == origen) && (c2 == destino)) = (obtDur vuelo):(conseguirEscalasCompletasAux vuelos t origen destino)
    | ((elem (c1) (destinosDelOrigen)) && (c2 == destino)) = ((obtDur vuelo) + buscarDuracion t origen c1):(conseguirEscalasCompletasAux vuelos t origen destino)
    | otherwise = (conseguirEscalasCompletasAux vuelos t origen destino)
    where c1 = obtCiudPrim vuelo
          c2 = obtCiudSeg vuelo
          destinosDelOrigen = conseguirDestinos t origen


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

