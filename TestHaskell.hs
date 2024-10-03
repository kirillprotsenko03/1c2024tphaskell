import Test.HUnit
import Funciones

run = runTestTT testGeneral
testGeneral = validezDuracionDelCaminoMasRapido

-- EJERCICIO 1: vuelosValidos ---------------------

validezVuelos = test [
    "misma salida y llegada" ~: vuelosValidos [("salta", "salta", 5), ("cordoba", "mendoza", 6)] ~?= False,
    "duración válida (1.1 - num neutro)" ~: vuelosValidos [("salta", "chubut", 5), ("cordoba", "mendoza", 0)] ~?= False,
    "duración válida (1.2 - num negativo)" ~: vuelosValidos [("salta", "chubut", 5), ("cordoba", "mendoza", -7)] ~?= False,
    "vuelos no repetidos" ~: vuelosValidos [("cordoba", "mendoza", 5), ("cordoba", "mendoza", 5), ("chubut", "mendoza", 3)] ~?= False,
    "diferente salida y llegada (2)" ~: vuelosValidos [("salta", "catamarca", 5), ("cordoba", "mendoza", 6)] ~?= True,
    "duración válida (2)" ~: vuelosValidos [("salta", "chubut", 5), ("cordoba", "mendoza", 6)] ~?= True,
    "vuelos no repetidos (2)" ~: vuelosValidos [("cordoba", "mendoza", 5), ("cordoba", "chubut", 5), ("chubut", "mendoza", 3)] ~?= True,
    "camino invertido" ~: vuelosValidos [("cordoba", "mendoza", 5), ("mendoza", "cordoba", 5), ("chubut", "mendoza", 3)] ~?= True
    -- ¿vale: (bs as, rosario, 7) (rosario, bs as, 4)?
    -- "camino invertido, y diferente duracion" ~: vuelosValidos [("cordoba", "mendoza", 5), ("mendoza", "cordoba", 5), ("chubut", "mendoza", 3)] ~?= False
 ]

-- EJERCICIO 2: ciudadesConectadas-------------------
validezCiudadesConectadas = test [
    "todas las opciones disponibles" ~: ciudadesConectadas [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "cordoba" ~?= ["mendoza", "salta", "chubut"],
-- como requiere vuelosValidos (no hay repes -> (bs, ros), (bs as, ros)) pero dice que no haya repetidos, supongo que admite (bs, ros), (ros, bs as)
    "sin repetidos" ~: ciudadesConectadas [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "cordoba" ~?= ["salta", "chubut", "mendoza"]
 ]


-- EJERCICIO 3: modernizarFlota ---------------------
validezModernizarFlota = test [
    "misma cantidad de vuelos" ~: modernizarFlota [("salta", "cordoba", 16), ("chubut", "mendoza", 100)] ~?= [("salta", "cordoba", 14.4), ("chubut", "mendoza", 90.0)],
    "todos los porcentajes correctos" ~: modernizarFlota [("chubut", "cordoba", 16), ("salta", "mendoza", 50)] ~?= [("chubut", "cordoba", 14.4), ("salta", "mendoza", 45.0)],
    "mismo origen y destino"~: modernizarFlota [("neuquen", "cordoba", 16), ("salta", "formosa", 50)] ~?= [("neuquen", "cordoba", 14.4), ("salta", "formosa", 45.0)]
 ]

-- EJERCICIO 4: ciudadMasConectada-------------------
validezCiudadMasConectada = test [
    "mayor conexion es correcta" ~: ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "mendoza", 3), ("formosa", "cordoba", 9)] ~?= "cordoba",
    "Varias Ciudade con mismas apariciones" ~: ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "mendoza", 3), ("formosa", "chubut", 9)] ~?= "cordoba"
 ]


-- EJERCICIO 5: sePuedeLlegar -----------------------
validezSePuedeLlegar = test [
    "vuelo directo" ~: sePuedeLlegar [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= True,
    "vuelo con escala" ~: sePuedeLlegar [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= True,
    "vuelo directo y con escala"~: sePuedeLlegar [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= True,
    "sin vuelos" ~: sePuedeLlegar [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "formosa", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= False
 ]


-- EJERCICIO 6: duracionDelCaminoMasRapido ----------
validezDuracionDelCaminoMasRapido = test [
    "vuelo directo es el más corto" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= 6.0,
    "vuelo con escala es el más corto" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 1), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 4)] "salta" "cordoba" ~?= 5.0,
    "solo hay vuelo directo" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= 6.0,
    "solo hay vuelo con escala" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= 11.0,
    -- ¿y si son iguales, se rompre o devuelve la misma duracion?
    "vuelo con escala y directo son iguales" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 1), ("formosa", "mendoza", 6), ("salta", "cordoba", 5), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 4)] "salta" "cordoba" ~?= 5.0
 ]


-- EJERCICIO 7: puedoVolverAOrigen -------------------
-- validezPuedoVolverAOrigen = test [
--     "varias conexiones, y unica ruta" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "cordoba", 6), ("cordoba", "formosa", 6), ("formosa", "chubut", 3), ("chubut", "salta", 4)] "salta" ~?= True,
--     "varias conexiones, y varias rutas" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "salta", 6), ("salta", "formosa", 6), ("formosa", "chubut", 3), ("chubut", "salta", 4)] "salta" ~?= True,
--     "ninguna conexión" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("formosa", "salta", 6), ("cordoba", "formosa", 6), ("catamarca", "chubut", 3), ("chubut", "formosa", 4)] "salta" ~?= False,
--     "ruta directa" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "salta", 6), ("cordoba", "formosa", 6), ("catamarca", "chubut", 3), ("chubut", "formosa", 4)] "salta" ~?= True
--  ]