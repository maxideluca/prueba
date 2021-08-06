data Jugador= CJugador {nombre::String,edad::Int,promediogol::Float,habilidad ::Int,cansancio::Float} deriving (Show,Eq)

data Equipo= UnEquipo {nombre2::String,grupo::Char,jugadores::[Jugador]}

martin::Jugador
martin = CJugador "Martin" 26 0.0 50 35.0
juan::Jugador
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi::Jugador
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0
jonathan::Jugador
jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean::Jugador
lean = CJugador "Hacha" 23 0.01 50 35.0
brian::Jugador
brian = CJugador "Panadero" 21 5 80 15.0
garcia::Jugador
garcia = CJugador "Sargento" 30 1 80 13.0
messi::Jugador
messi = CJugador "Pulga" 26 10 99 43.0
aguero::Jugador
aguero = CJugador "Aguero" 24 5 90 5.0

equipo1::Equipo
equipo1 = UnEquipo "Lo Que Vale Es El Intento" 'F' [martin, juan, maxi]
losDeSiempre::Equipo
losDeSiempre = UnEquipo "Los De Siempre" 'F' [jonathan, lean, brian]
restoDelMundo::Equipo
restoDelMundo =UnEquipo "Resto del Mundo" 'A' [garcia, messi, aguero]

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

sonfigura:: Equipo->[Jugador]
sonfigura unequipo= filter esbueno (jugadores unequipo)

esbueno:: Jugador->Bool
esbueno unjugador= ((>75).habilidad)unjugador && ((>0).promediogol)unjugador

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero:: Equipo->Bool
tieneFarandulero= (any esfarandulero).jugadores

esfarandulero:: Jugador->Bool
esfarandulero unjugador= elem (nombre unjugador) jugadoresFaranduleros

figuritadificil:: [Equipo]->Char->[Jugador]
figuritadificil listaequipos grupoo= filter dificiljugador (jugadoresdeequiposdeungrupo grupoo listaequipos)

dificiljugador :: Jugador->Bool
dificiljugador unjugador= esbueno unjugador && ((<27).edad)unjugador && (not.esfarandulero)unjugador

jugadoresdeequiposdeungrupo:: Char->[Equipo]->[Jugador]
jugadoresdeequiposdeungrupo n listaequipos=concatMap jugadores (filter ((==n).grupo)listaequipos)

quiengana:: Equipo->Equipo->Equipo
quiengana equipo1 equipo2 | funcion1 equipo1>funcion1 equipo2= equipo1
                          | funcion1 equipo1<funcion1 equipo2= equipo2 

funcion1 :: Equipo->Float
funcion1= sum.(map promediogol).(take 11).quickSort (\jugador1 jugador2 ->cansancio jugador1<cansancio jugador2).jugadores 