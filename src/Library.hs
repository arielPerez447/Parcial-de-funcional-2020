module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-------------------------------------------------------------
--                   Recuperatorio PdeP
-------------------------------------------------------------
data Serie = Serie{
   nombreSerie:: String,
   actores :: [Actor],
   presupuestoAnual:: Number,
   cantidadTemporadas :: Number,
   promedioRating:: Number,
   estaCancelada ::Bool
}deriving(Show,Eq)

data Actor = Actor{
   nombreActor :: String,
   sueldoAnual ::Number,
   restricciones :: [String]
}deriving(Show, Eq)

---Punto 1 
----Parte 1
estaEnRojo :: Serie -> Bool
estaEnRojo serie = presupuestoAnual serie <  foldr ((+) . sueldoAnual) 0  (actores serie)

----Parte 2
esProblematica :: Serie -> Bool
esProblematica serie = any (\unActor -> length (restricciones unActor) > 1 ) (actores serie)

---Punto 2

type Productor = Serie->Serie

----Parte 1 
conFavoritismo :: [Actor] -> Productor
conFavoritismo actoresFavoritos serie = serie{actores= drop 2 (actores serie) ++ actoresFavoritos}

----Parte 2
timBurton :: Productor
timBurton = conFavoritismo [johnnyDepp, helenaBonham]

----Parte 3
gatoPardeitor :: Productor
gatoPardeitor = id

----Parte 4
estireior :: Productor
estireior serie = serie {cantidadTemporadas = 2 * cantidadTemporadas serie}

----Parte 5
desespereitor :: Productor
desespereitor  = gatoPardeitor.estireior

----Parte 6
canceleitor :: Number -> Serie -> Serie
canceleitor rating serie
   |estaEnRojo serie || promedioRating serie < rating  = serie {estaCancelada = False}
   |otherwise        = serie

actoresConRestricciones :: Serie -> [Actor]
actoresConRestricciones serie = filter(\unActor -> length (restricciones unActor) > 1 ) (actores serie)

cantidadActores :: Serie -> Number
cantidadActores = length.actores

---Punto 3
bienestar :: Serie -> Number
bienestar serie
   |estaCancelada serie = (+) (sumatoriaPorTemporadas serie)  (sumatoriaPorActores serie)
   |otherwise            = 0

sumatoriaPorTemporadas :: Serie -> Number
sumatoriaPorTemporadas serie
   |cantidadTemporadas serie > 4 = 5
   | otherwise                   = 10 - cantidadTemporadas serie * 2

sumatoriaPorActores :: Serie -> Number
sumatoriaPorActores serie
   |cantidadActores serie < 10 = 3
   |otherwise                  = min 2 (10 - (length.actoresConRestricciones) serie)


---Punto 4
mayorBienestar :: [Productor] -> [Serie] -> Productor
mayorBienestar [productor] _ = productor
mayorBienestar (primerProductor : segundoProductor : productores ) (serie : series)
   |(bienestar.primerProductor)serie > (bienestar.segundoProductor)serie = primerProductor
   |otherwise                                                            = mayorBienestar (segundoProductor: productores) series

--Punto 5
twd :: Serie
twd = Serie "twd" infinitosActores 310000000 10 55 True
---Parte 1
infinitosActores :: [Actor]
infinitosActores = repeat paulRudd

---Parte 2
---- Si, si puede dar un resultado pero no se veria reflejado por tener una lista infinita de actores pero haskell usa evaluacion diferida
---- puede satisfacer la función tim burton

---Parte 3
---- No, debido a que para poder resolver la función estaEnrojo necesita saber los sueldos de todos los actores de la serie 

---Punto 6

esControvertida :: Serie -> Bool
esControvertida = not.parteRecursiva.actores

parteRecursiva :: [Actor] -> Bool
parteRecursiva [actor] = True
parteRecursiva (primerActor:segundoActor:actores) = sueldoAnual primerActor < sueldoAnual segundoActor && parteRecursiva (segundoActor:actores)


---Para probar las funciones 
friends :: Serie
friends = Serie "friends" [paulRudd, helenaBonham, johnnyDepp] 5000000000 10 56 True
paulRudd :: Actor
paulRudd = Actor "paul rudd" 41000000 ["no actuar en bata", "comer ensalada"]

johnnyDepp :: Actor
johnnyDepp = Actor "jonny Depp" 20000000 []

helenaBonham :: Actor
helenaBonham = Actor "helena Bonham" 15000000 []