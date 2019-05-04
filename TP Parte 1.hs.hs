import Text.Show.Functions
data Auto = Auto {nombre :: String, nivelNafta :: Int, velocidad :: Int , nombreEnamorade :: String, truco :: Truco} deriving (Show)
type Truco = Auto -> Auto

data Carrera = Carrera {nombreCarrera :: String, largoPista :: Int, cantidadVueltas :: Int, publico :: [String], participantes :: [Auto], trampa :: Trampa} deriving (Show)
type Trampa = Carrera -> Carrera

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr = Auto "Biankerr" 500 20 "Tinch" impresionar
gusthav = Auto "Gusthav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor ("Petra"))

potreroFunes = Carrera "potreroFunes" 5 3 ["Ronco", "Tinch", "Dodain"] [rochaMcQueen, biankerr, gusthav, rodra] sacarAlPistero

cambiarVelocidad unAuto incremento = unAuto {velocidad=incremento.velocidad $unAuto}

deReversa :: Truco
deReversa unAuto = unAuto {nivelNafta = div (velocidad unAuto) 5 + nivelNafta unAuto}

impresionar :: Truco
impresionar unAuto = cambiarVelocidad unAuto (*2)

nitro :: Truco
nitro unAuto = cambiarVelocidad unAuto (+15)

fingirAmor :: String -> Truco
fingirAmor enamoradeConveniencia unAuto = unAuto {nombreEnamorade = enamoradeConveniencia}

cantidadVocalesEnamorade :: Auto -> Int
cantidadVocalesEnamorade unAuto = length.filter (\letra -> elem letra "aeiouAEIOU").nombreEnamorade $unAuto

incrementarVelocidad :: Truco
incrementarVelocidad unAuto
    |cantidadVocalesEnamorade unAuto <=2 = cambiarVelocidad unAuto (+15)
    |cantidadVocalesEnamorade unAuto <=4 = cambiarVelocidad unAuto (+20)
    |cantidadVocalesEnamorade unAuto >4 = cambiarVelocidad unAuto (+30)

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((>0).nivelNafta) unAuto && ((<100).velocidad) unAuto

comboLoco :: Truco
comboLoco = deReversa.nitro 

queTrucazo :: String -> Truco
queTrucazo enamoradeConveniencia = incrementarVelocidad.fingirAmor enamoradeConveniencia

turbo :: Truco
turbo unAuto = unAuto {velocidad = ((+velocidad unAuto).(*10).nivelNafta) unAuto , nivelNafta = 0} 

trampaBase unaCarrera funcion = unaCarrera {participantes = funcion.participantes $unaCarrera}

sacarAlPistero :: Trampa
sacarAlPistero unaCarrera = trampaBase unaCarrera (drop 1)

lluvia :: Truco
lluvia unAuto = cambiarVelocidad unAuto ((-)(10))

lluviaTrampa :: Trampa
lluviaTrampa unaCarrera = trampaBase unaCarrera (map(lluvia))

inutilidad :: Truco
inutilidad unAuto = unAuto

neutralizarTrucos :: Trampa
neutralizarTrucos unaCarrera = trampaBase unaCarrera (map(inutilidad))

pocaReserva :: Auto -> Bool
pocaReserva = (>30).nivelNafta

pocaReservaTrampa :: Trampa
pocaReservaTrampa unaCarrera = trampaBase unaCarrera (filter(pocaReserva))

podio :: Trampa
podio unaCarrera = trampaBase unaCarrera (take 3)

restarNafta :: Carrera -> Truco
restarNafta unaCarrera unAuto = unAuto {nivelNafta = nivelNafta unAuto - (div (largoPista unaCarrera) 10) * (velocidad unAuto)}

restarNaftaParticipantes :: [Auto] -> Carrera -> Carrera
restarNaftaParticipantes autos unaCarrera = unaCarrera {participantes = map(restarNafta unaCarrera) autos}

suEnamoradeEstaEnElPublico :: Carrera -> Auto -> Bool
suEnamoradeEstaEnElPublico unaCarrera unAuto = elem (nombreEnamorade unAuto) (publico unaCarrera)

realizarTruco :: Auto -> Auto
realizarTruco unAuto = truco unAuto unAuto

realizaSuTruco :: Trampa
realizaSuTruco unaCarrera = unaCarrera {participantes = (map(realizarTruco).filter(suEnamoradeEstaEnElPublico unaCarrera).participantes $unaCarrera) 
++ (filter(not.suEnamoradeEstaEnElPublico unaCarrera).participantes $unaCarrera)}

darVuelta :: Carrera -> Carrera
darVuelta unaCarrera = trampa(unaCarrera).realizaSuTruco.restarNaftaParticipantes(participantes unaCarrera) $unaCarrera

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera = iterate darVuelta unaCarrera !! (cantidadVueltas unaCarrera)

velocidadMaxima :: Carrera -> Int
velocidadMaxima unaCarrera = maximum.map(velocidad).participantes $ (correrCarrera unaCarrera)
