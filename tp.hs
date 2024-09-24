type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]


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

durDistCiudsIguales :: Vuelo -> Vuelo -> Bool
durDistCiudsIguales vuelo1 vuelo2 = (ciudPrimIguales vuelo1 vuelo2) &&
                              (ciudSegIguales vuelo1 vuelo2)



hayVuelosRepetidos :: AgenciaDeViajes -> Bool
hayVuelosRepetidos [] = False
hayVuelosRepetidos (vuelo:[]) = False
hayVuelosRepetidos (vuelo:vuelos) = (elem vuelo vuelos) ||
                                    (hayVuelosRepetidos vuelos) 

hayDurDistCiudsIguales :: AgenciaDeViajes -> Bool
hayDurDistCiudsIguales [] = False
hayDurDistCiudsIguales (vuelo:[]) = False
hayDurDistCiudsIguales (vuelo:vuelos)


-- ejercicio 1 ------

vueloValido :: Vuelo -> Bool
vueloValido vuelo = (obtDur vuelo > 0) && 
                    (obtCiudPrim vuelo /= obtCiudSeg vuelo)

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True











