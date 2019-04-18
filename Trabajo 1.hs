data Auto = Auto {nombre :: String, nivelNafta :: Int, velocidad :: Int , nombreEnamorade :: String, truco :: String} deriving (Show)

deReversa :: Auto -> Auto
deReversa unAuto = unAuto {nivelNafta = ((+200).nivelNafta) unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = ((*2).velocidad) unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = ((+15).velocidad) unAuto}

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto enamoradeConveniencia = unAuto {nombreEnamorade = enamoradeConveniencia}

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" "deReversa"
biankerr = Auto "Biankerr" 500 20 "Tinch" "impresionar" 
gusthav = Auto "Gusthav" 200 130 "PetiLaLinda" "nitro"
rodra = Auto "Rodra" 0 50 "Taisa" "fingirAmor"


