import Text.Show.Functions
data Auto = Auto {nombre :: String, nivelNafta :: Int, velocidad :: Int , nombreEnamorade :: String, truco :: Auto -> Auto} deriving Show
type Truco = Auto -> Auto

deReversa :: Truco
deReversa unAuto = unAuto {nivelNafta = ((+200).nivelNafta) unAuto}

cambiarVelocidad unAuto incremento = unAuto {velocidad=incremento.velocidad $unAuto}

impresionar :: Truco
impresionar unAuto = cambiarVelocidad unAuto (*2)

nitro :: Truco
nitro unAuto = cambiarVelocidad unAuto (+15)

fingirAmor :: String -> Truco
fingirAmor unAuto enamoradeConveniencia = unAuto {nombreEnamorade = enamoradeConveniencia}

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr = Auto "Biankerr" 500 20 "Tinch" impresionar
gusthav = Auto "Gusthav" 200 130 "PetiLaLinda" nitro
--rodra = Auto "Rodra" 0 50 "Taisa" fingirAmor "Petra"

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
