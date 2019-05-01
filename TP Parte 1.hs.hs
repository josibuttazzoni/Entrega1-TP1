import Text.Show.Functions
data Auto = Auto {nombre :: String, nivelNafta :: Int, velocidad :: Int , nombreEnamorade :: String, truco :: Truco} deriving (Show)
type Truco = Auto -> Auto

data Carrera = Carrera {nombreCarrera :: String, largoPista :: Float, cantidadVueltas :: Int, publico :: [String], participantes :: [Auto], trampa :: Trampa} deriving (Show)
type Trampa = Carrera -> Carrera


rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr = Auto "Biankerr" 500 20 "Tinch" impresionar
gusthav = Auto "Gusthav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor ("Petra"))

potreroFunes = Carrera "potreroFunes" 5.0 3 ["Ronco", "Tinch", "Dodain"] [rochaMcQueen, biankerr, gusthav, rodra] sacarAlPistero

cambiarVelocidad unAuto incremento = unAuto {velocidad=incremento.velocidad $unAuto}

deReversa :: Truco
deReversa unAuto = unAuto {nivelNafta = div (velocidad unAuto) 5 + nivelNafta unAuto}

impresionar :: Truco
impresionar unAuto = cambiarVelocidad unAuto (*2)

nitro :: Truco
nitro unAuto = cambiarVelocidad unAuto (+15)

fingirAmor :: String -> Truco
fingirAmor enamoradeConveniencia unAuto = unAuto {nombreEnamorade = enamoradeConveniencia}

cantidadVocales :: Auto -> Int
cantidadVocales unAuto = length.filter (\letra -> elem letra "aeiouAEIOU").nombreEnamorade $unAuto

incrementarVelocidad :: Truco
incrementarVelocidad unAuto
    |cantidadVocales unAuto <=2 = cambiarVelocidad unAuto (+15)
    |cantidadVocales unAuto <=4 = cambiarVelocidad unAuto (+20)
    |cantidadVocales unAuto >4 = cambiarVelocidad unAuto (+30)

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((>0).nivelNafta) unAuto && ((<100).velocidad) unAuto

comboLoco :: Truco
comboLoco = deReversa.nitro 

queTrucazo :: String -> Truco
queTrucazo enamoradeConveniencia = incrementarVelocidad.fingirAmor enamoradeConveniencia

turbo :: Truco
turbo unAuto = unAuto {velocidad = ((+velocidad unAuto).(*10).nivelNafta) unAuto , nivelNafta = 0}

sacarAlPistero :: Trampa
sacarAlPistero unaCarrera = unaCarrera {participantes = (drop 1).participantes $unaCarrera}

lluvia :: Truco
lluvia unAuto = unAuto {velocidad=velocidad unAuto - 10}

lluviaTrampa :: Trampa
lluviaTrampa unaCarrera = unaCarrera {participantes=map(lluvia).participantes $unaCarrera}

inutilidad :: Truco
inutilidad unAuto = unAuto

neutralizarTrucos :: Trampa
neutralizarTrucos unaCarrera = unaCarrera {participantes = map(inutilidad).participantes $unaCarrera}

pocaReserva :: Auto -> Bool
pocaReserva = (>30).nivelNafta

pocaReservaTrampa :: Trampa
pocaReservaTrampa unaCarrera = unaCarrera {participantes = filter(pocaReserva).participantes $unaCarrera}

podio :: Trampa
podio unaCarrera = unaCarrera {participantes = take 3.participantes $unaCarrera}
