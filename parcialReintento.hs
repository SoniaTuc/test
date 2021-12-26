module Main where
import Lib
import Data.List

main :: IO ()
main = putStrLn "parcialReintento"

{--1--}

data Autobot = CAutobot String Float [String] Int [Arma] | CDecepticon String Float [Arma] deriving Show

data Arma=Arma String Float deriving Show

canion30Cm=Arma "Canion 30 cm" 15
canion50Cm=Arma "Canion 50 cm" 25
bumbleebe=CAutobot "Bumbleebe" 400 ["Amarillo"] 300 [canion30Cm,canion50Cm]

pistolaCalibre80= Arma "Pistola Calibre 80" 40
topSpin= CAutobot "Topspin" 600 ["Azul", "Blanco"] 500  [pistolaCalibre80]

hacha=Arma "Hacha" 100
espada=Arma "Espada" 200
optimusPrime=CAutobot "Optimus Prime" 1000 [] 700 [hacha,espada]

megatron=CDecepticon "Megatron" 700 [hacha,canion50Cm]
diablo=CDecepticon "Diablo" 1000 [espada,canion50Cm]

{--2--}

nombre (CAutobot nombre _ _ _ _)=nombre
nombre (CDecepticon nombre _ _ )=nombre
vida (CAutobot _ vida _ _ _)=vida
vida (CDecepticon _ vida _ )=vida
edad(CAutobot _ _ _ edad _)=edad
edad (CDecepticon nombre _ _ )=(*100)(length nombre)
colores(CAutobot _ _ colores _ _)=colores
colores(CDecepticon _ _ _ )=[]

armas (CAutobot _ _ _ _ armas)=armas
armas(CDecepticon _ _ armas )=armas

nombreArma(Arma nombre _)=nombre
danioArma(Arma _ danio)=danio

{--3 Hecho arriba--}-- lo unico que no se modifica son las funciones respecto a la data Armas, ya que se usa el mismo constructor Arma para ambos tipos de transformers 

{--4--}


poderDeAtaque (CAutobot _ _ colores edad armas)=(+ (danioArmas armas * extraAtaque colores edad))(danioArmas armas)
poderDeAtaque(CDecepticon _ _ armas)=sum (map danioArma armas)

danioArmas armas=sum(map danioArma armas)

extraAtaque colores edad| edad>10 || elem "Rojo" colores = 0.5
                      |otherwise=0


{--5--}


ataqueEnGrupo listaPersonajes enemigo=atacarNVecesEnGrupo listaPersonajes enemigo 1
atacarNVecesEnGrupo listaAtacantes atacado n=foldl (\acum atacante->serAtacado acum atacante  n) atacado listaAtacantes

serAtacado (CDecepticon nombre vida armas) atacante n=(CDecepticon nombre (vida-(poderDeAtaque atacante* n)) armas)
serAtacado (CAutobot nombre vida colores anios armas) atacante n=(CAutobot nombre (vida -(poderDeAtaque atacante *n)) colores anios armas)

{--6--}
--La función deberá retornar un valor booleano indicando si los enemigos soportaron esa cantidad de ataques.
-- La condición para saber si resistieron es que, después de haber sufrido esos "n" ataques,
 --todavía existe algún enemigo que tenga vida mayor a 0. Los enemigos y aliados pueden ser tanto Autobots como Decepticons.

soportanNAtaquesEnGrupo :: [Autobot] -> [Autobot] -> Float -> Bool
soportanNAtaquesEnGrupo listaAliados listaEnemigos n=any ((>0).vida) (guerra listaAliados listaEnemigos n)

guerra :: Foldable t => t Autobot -> [Autobot] -> Float -> [Autobot]
guerra listaAliados listaEnemigos n =[atacarNVecesEnGrupo listaAliados enemigo n|enemigo<-listaEnemigos]


grupo1=[bumbleebe, optimusPrime]
grupo2=[megatron,diablo]

{--7--}

estadistica condicion transformacion = (map transformacion). (filter condicion)


--a condicion transformacion  ((>3).length.colores)  (map nombreArma.armas) transformer
--b condicion transformacion  ((>800).edad)  edad   
--c condicion transformacion  ((>3).length.filter (\arma-> (>10)(length(nombreArma arma))).armas)  nombre

