type Ciudad = String
type Duracion = Float
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

conseguirDuraciones :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Int]
conseguirDuraciones [] _ _ = 0
conseguirDuraciones (ciudad:ciudades) origen destino
    | origen == (obtCiudPrim ciudad) && (destino == obtCiudSeg ciudad) = obtDur ciudad


-- ejercicio 7--

