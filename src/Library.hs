module Library where
import PdePreludat
import GHC.IO.Windows.Handle (HANDLE)
import GHC.Generics (prec)
import GHC.Arr (listArray)
import Data.List (delete)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | PatiVegano | PancetaDeTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente PancetaDeTofu = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precioFinal :: Hamburguesa -> Number
precioFinal hamb = precioBase hamb + sum (map precioIngrediente (ingredientes hamb))


agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamb =
    hamb {ingredientes = ingrediente : ingredientes hamb}

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | Carne `elem` ingredientes hamburguesa = hamburguesa {ingredientes = Carne : ingredientes hamburguesa } -- no se podría usar la función agregarIngrediente para hacer más declarativo esto?
    | Pollo `elem` ingredientes hamburguesa = hamburguesa {ingredientes = Pollo : ingredientes hamburguesa}
    | PatiVegano `elem` ingredientes hamburguesa = hamburguesa {ingredientes = PatiVegano : ingredientes hamburguesa}
    | otherwise = hamburguesa { precioBase = precioBase hamburguesa + 20, ingredientes = Carne : ingredientes hamburguesa }



descuento :: Number -> Hamburguesa -> Hamburguesa
descuento des hamb = hamb{precioBase = precioBase hamb - (precioBase hamb * des / 100)}

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

pdepBurguer :: Hamburguesa
pdepBurguer = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

-- dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agrandar $ cuartoDeLibra


reemplazarIngredientePorOtro :: Ingrediente -> Ingrediente -> Hamburguesa -> Hamburguesa 
reemplazarIngredientePorOtro reemplazado sustituto hamburguesa = hamburguesa {ingredientes = replicate (length (filter (/= reemplazado) (ingredientes hamburguesa))) sustituto ++ filter (/= reemplazado) (ingredientes hamburguesa)}


hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = reemplazarIngredientePorOtro Carne PatiVegano . reemplazarIngredientePorOtro Pollo PatiVegano . reemplazarIngredientePorOtro Cheddar QuesoDeAlmendras . reemplazarIngredientePorOtro Panceta PancetaDeTofu

cambiarPandePati :: Hamburguesa -> Hamburguesa
cambiarPandePati = reemplazarIngredientePorOtro Pan PanIntegral

dobleCuartoVeggie :: Hamburguesa
dobleCuartoVeggie = hacerVeggie . cambiarPandePati $ dobleCuarto