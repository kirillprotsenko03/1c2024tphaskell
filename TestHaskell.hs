import Test.HUnit
import Solucion
import Data.List

-- run = runTestTT testGeneral
-- testGeneral = validezCiudadesConectadas

allTests :: Test
allTests = test [
    validezVuelos,
    validezCiudadesConectadas,
    validezModernizarFlota,
    validezCiudadMasConectada,
    validezSePuedeLlegar,
    validezDuracionDelCaminoMasRapido,
    validezPuedoVolverAOrigen
 ]
run = runTestTT allTests


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
 ]

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)

-- EJERCICIO 2: ciudadesConectadas-------------------
validezCiudadesConectadas = test [
    "todas las opciones disponibles" ~: expectPermutacion (ciudadesConectadas [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "cordoba") ["salta", "chubut", "mendoza"],
    "sin repetidos" ~: expectPermutacion (ciudadesConectadas [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "cordoba") ["mendoza","salta", "chubut"],
    "única conexión" ~: expectPermutacion (ciudadesConectadas [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "mendoza", 3)] "cordoba") ["salta"],
    "único vuelo" ~: expectPermutacion (ciudadesConectadas [("salta", "cordoba", 6)] "cordoba") ["salta"],
    "varias conexiones" ~: expectPermutacion (ciudadesConectadas [("rio negro", "cordoba", 6), ("formosa", "cordoba", 6), ("cordoba", "catamarca", 6)] "cordoba") ["rio negro", "formosa", "catamarca"],
    "agencia vacia" ~: ciudadesConectadas [] "cordoba" ~?= [],
    "ciudad no existente" ~: ciudadesConectadas [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("catamarca", "cordoba", 6)] "formosa" ~?= []
 ]



-- EJERCICIO 3: modernizarFlota ---------------------
validezModernizarFlota = test [
    "misma cantidad de vuelos" ~: modernizarFlota [("salta", "cordoba", 16), ("chubut", "mendoza", 100)] ~?= [("salta", "cordoba", 14.4), ("chubut", "mendoza", 90.0)],
    "todos los porcentajes correctos" ~: modernizarFlota [("chubut", "cordoba", 16), ("salta", "mendoza", 50)] ~?= [("chubut", "cordoba", 14.4), ("salta", "mendoza", 45.0)],
    "mismo origen y destino"~: modernizarFlota [("neuquen", "cordoba", 16), ("salta", "formosa", 50)] ~?= [("neuquen", "cordoba", 14.4), ("salta", "formosa", 45.0)]
 ]


-- EJERCICIO 4: ciudadMasConectada-------------------
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

validezCiudadMasConectada = test [
    "mayor conexion es correcta" ~: ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "mendoza", 3), ("formosa", "cordoba", 9)] ~?= "cordoba",
    {-nuevo-}
    "unico vuelo" ~: expectAny (ciudadMasConectada [("salta", "cordoba", 5)]) ["cordoba", "salta"],
    "empate vuelos entre 3" ~: expectAny (ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "salta", 3), ("cordoba", "chubut", 9)]) ["salta", "cordoba", "chubut"],
    "empate vuelos entre 2" ~: expectAny (ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "salta", 3), ("cordoba", "chubut", 9), ("salta", "mendoza", 7), ("chubut", "mendoza", 5)]) ["chubut", "salta"],
    "funcionamiento correcto" ~: ciudadMasConectada [("salta", "cordoba", 5), ("chubut", "salta", 3), ("cordoba", "chubut", 9), ("salta", "mendoza", 7)] ~?= "salta"
 ]


-- EJERCICIO 5: sePuedeLlegar -----------------------
validezSePuedeLlegar = test [
    "vuelo directo" ~: sePuedeLlegar [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= True,
    "vuelo con escala" ~: sePuedeLlegar [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= True,
    "vuelo directo y con escala"~: sePuedeLlegar [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= True,
    "sin vuelos" ~: sePuedeLlegar [("salta", "mendoza", 5), ("cordoba", "mendoza", 6), ("salta", "formosa", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= False,
    "agencia vacia" ~: sePuedeLlegar [] "salta" "cordoba" ~?= False
 ]


-- EJERCICIO 6: duracionDelCaminoMasRapido ----------
validezDuracionDelCaminoMasRapido = test [
    "vuelo directo es el más corto" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= 6.0,
    "vuelo con escala es el más corto" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 1), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 4)] "salta" "cordoba" ~?= 5.0,
    "solo hay vuelo directo" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("salta", "cordoba", 6), ("chubut", "cordoba", 3)] "salta" "cordoba" ~?= 6.0,
    "solo hay vuelo con escala" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 5), ("formosa", "mendoza", 6), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 6)] "salta" "cordoba" ~?= 11.0,
    "vuelo con escala y directo son iguales" ~: duracionDelCaminoMasRapido [("salta", "mendoza", 1), ("formosa", "mendoza", 6), ("salta", "cordoba", 5), ("chubut", "cordoba", 3), ("mendoza", "cordoba", 4)] "salta" "cordoba" ~?= 5.0
 ]


-- EJERCICIO 7: puedoVolverAOrigen -------------------
validezPuedoVolverAOrigen = test [
    "varias conexiones, y unica ruta" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "cordoba", 6), ("cordoba", "formosa", 6), ("formosa", "chubut", 3), ("chubut", "salta", 4)] "salta" ~?= True,
    "varias conexiones, y varias rutas" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "salta", 6), ("salta", "formosa", 6), ("formosa", "chubut", 3), ("chubut", "salta", 4)] "salta" ~?= True,
    "ninguna conexión" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("formosa", "salta", 6), ("cordoba", "formosa", 6), ("catamarca", "chubut", 3), ("chubut", "formosa", 4)] "salta" ~?= False,
    "ruta directa" ~: puedoVolverAOrigen [("salta", "mendoza", 1), ("mendoza", "salta", 6), ("cordoba", "formosa", 6), ("catamarca", "chubut", 3), ("chubut", "formosa", 4)] "salta" ~?= True
 ]
